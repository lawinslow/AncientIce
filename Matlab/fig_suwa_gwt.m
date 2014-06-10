
d = importdata('../Data/SuwaEarly101.txt');

suwa = detrend(d.data(:,3));
time = d.data(:,1);

addpath('wtc');

%%
figure(1);
set(gcf,'color','white');
data = suwa;

variance = std(data)^2;
data = (data - mean(data))/sqrt(variance) ;

n = length(data);
dt = 1;
xlim = [1580,1680];  % plotting range
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


whos

%------------------------------------------------------ Plotting

%--- Plot time series
subplot('position',[0.1 0.75 0.65 0.2])
plot(time, 0, 'k--');
hold on;
plot(time,suwa);
set(gca,'XLim',xlim(:))
xlabel('Year')
ylabel('Ice off anomaly (year)')
title('a) Suwa ice-off anomaly')
hold off

%--- Contour plot wavelet power spectrum
subplot('position',[0.1 0.37 0.65 0.28])
levels = [0.0625,0.125,0.25,0.5,1,2,4,8,16] ;
Yticks = 2.^(fix(log2(min(period))):fix(log2(max(period))));
contourf(time,log2(period),log2(power),log2(levels),'linestyle','none');  %*** or use 'contourfill'
%imagesc(time,log2(period),log2(power));  %*** uncomment for 'image' plot
xlabel('Year')
ylabel('Period (years)')
title('b) Wavelet Power Spectrum')
set(gca,'XLim',xlim(:))
set(gca,'YLim',log2([min(period),max(period)]), ...
	'YDir','reverse', ...
	'YTick',log2(Yticks(:)), ...
	'YTickLabel',Yticks)
% 95% significance contour, levels at -99 (fake) and 1 (95% signif)
hold on
contour(time,log2(period),sig95,[-99,1],'k', 'linewidth', 2);
hold on
% cone-of-influence, anything "below" is dubious
plot(time,log2(coi),'k')
hold off

%--- Plot global wavelet spectrum
subplot('position',[0.77 0.37 0.2 0.28])
plot(global_ws,log2(period))
hold on
plot(global_signif,log2(period),'--')
hold off
xlabel('Power (day^2)')
title('c) Global Wavelet Spectrum')
set(gca,'YLim',log2([min(period),max(period)]), ...
	'YDir','reverse', ...
	'YTick',log2(Yticks(:)), ...
	'YTickLabel','')
set(gca,'XLim',[0,1.25*max(global_ws)])

%--- Plot 2--8 yr scale-average time series
subplot('position',[0.1 0.07 0.65 0.2])
plot(time,scale_avg)
set(gca,'XLim',xlim(:))
xlabel('Year')
ylabel('Avg variance (day^2)')
title('d) Scale-specific average time series')
hold on;
plot(time,scale_avg_119, 'r');
plot(time,scale_avg_32,'g');

plot(xlim,scaleavg_signif+[0,0],'--')
plot(xlim,scaleavg_signif_119+[0,0],'r--')
plot(xlim,scaleavg_signif_32+[0,0],'g--')
legend({'2-6', '6-12', '16-32'});
hold off

%% export
export_fig('../Figures/SuwaEarlyOverallWavelet.tiff','-r300');
