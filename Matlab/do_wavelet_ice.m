function [period, power, sig95, coi, global_ws, global_signif]= do_wavelet_ice(data)

data(isnan(data)) = max(data(~isnan(data)));

variance = std(data(~isnan(data)))^2;
data = (data - mean(data(~isnan(data))))/sqrt(variance) ;

n = length(data);
dt = 1;


pad = 1;      % pad the time series with zeroes (recommended)
dj = 0.25;    % this will do 4 sub-octaves per octave
s0 = 2*dt;    % this says start at a scale of 6 months
j1 = 7/dj;    % this says do 7 powers-of-two with dj sub-octaves each
lag1 = 0;  % lag-1 autocorrelation for red noise background
mother = 'Morlet';

% Wavelet transform:
[wave,period,scale,coi] = wavelet(data,dt,pad,dj,s0,j1,mother);
power = (abs(wave)).^2 ;        % compute wavelet power spectrum

% Significance levels: (variance=1 for the normalized data)
[signif,fft_theor] = wave_signif(1.0,dt,scale,0,lag1,-1,-1,mother);
sig95 = (signif')*(ones(1,n));  % expand signif --> (J+1)x(N) array
sig95 = power ./ sig95;         % where ratio > 1, power is significant

% Global wavelet spectrum & significance levels:
global_ws = variance*(sum(power')/n);   % time-average over all times
dof = n - scale;  % the -scale corrects for padding at edges
global_signif = wave_signif(variance,dt,scale,1,lag1,-1,dof,mother);

% Scale-average between 2-6 years
avg = find((scale >= 2) & (scale < 6));
Cdelta = 0.776;   % this is for the MORLET wavelet
scale_avg = (scale')*(ones(1,n));  % expand scale --> (J+1)x(N) array
scale_avg = power ./ scale_avg;   % [Eqn(24)]
scale_avg = variance*dj*dt/Cdelta*sum(scale_avg(avg,:));   % [Eqn(24)]
scaleavg_signif = wave_signif(variance,dt,scale,2,lag1,-1,[2,5.9],mother);

% Scale-average between 6-12 years
avg = find((scale >= 6) & (scale < 12));
Cdelta = 0.776;   % this is for the MORLET wavelet
scale_avg_119 = (scale')*(ones(1,n));  % expand scale --> (J+1)x(N) array
scale_avg_119 = power ./ scale_avg_119;   % [Eqn(24)]
scale_avg_119 = variance*dj*dt/Cdelta*sum(scale_avg_119(avg,:));   % [Eqn(24)]
scaleavg_signif_119 = wave_signif(variance,dt,scale,2,lag1,-1,[2,11.9],mother);


% Scale-average between 6-12 years
avg = find((scale >= 16) & (scale < 32));
Cdelta = 0.776;   % this is for the MORLET wavelet
scale_avg_32 = (scale')*(ones(1,n));  % expand scale --> (J+1)x(N) array
scale_avg_32 = power ./ scale_avg_32;   % [Eqn(24)]
scale_avg_32= variance*dj*dt/Cdelta*sum(scale_avg_32(avg,:));   % [Eqn(24)]
scaleavg_signif_32 = wave_signif(variance,dt,scale,2,lag1,-1,[16,31.9],mother);

end