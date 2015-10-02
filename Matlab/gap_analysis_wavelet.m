
bumpleft = -0.6;
bumpright = 0.35;
set(0, 'DefaultAxesFontName', 'Helvetica')
set(0,'DefaultTextFontname', 'Helvetica')

%%

%d = importdata('../Data/Other/SuwaEarly101.txt');
%suwa = detrend(d.data(:,3));
%time = d.data(:,1);

f = fopen('../Data/suwa.old.tsv');
%header = strsplit(fgetl(f), '\t');
header = regexp(fgetl(f), '\t', 'split');
d = textscan(f, '%f%f%f%f%f%f%f%f%f%f', 'Delimiter','\t', 'TreatAsEmpty', 'NA');
fclose(f);

d = [d{:}];

time = d(:,1);
suwa = (d(:,4));

addpath('wtc');


%% create artificial signal

nanperc = 0.2;

x = 1:length(suwa);
x = x';

y = 0.4*sin(2*pi*x/20) + 0.2*sin(2*pi*x/5) + 0.2*sin(2*pi*x/1) + 0.2*randn(1,length(x))';

ally = y;
%y(randi(length(y), floor(nanperc*length(y)),1)) = NaN;

%y(isnan(torn)) = NaN;

y(isnan(y)) = max(y(~isnan(y)));
%y(isnan(y)) = 0;

%%
cutoff_year = -1;

data = y;

variance = std(data(~isnan(data)))^2;
data = (data - mean(data(~isnan(data))))/sqrt(variance) ;

n = length(data);
dt = 1;
xlim = [1443,2013];  % plotting range

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

%------------------------------------------------------ Plotting

figure(1);
set(gcf,'color','white', 'position',[24 54 1239 553]);
%--- Contour plot wavelet power spectrum
subleft = 0.05+0.4*(min(xlim)-1443)/(2013-1443);
subright = 0.4*range(xlim)/(2013-1443);
subplot('position',[subleft+bumpright 0.1 subright 0.4])

levels = [0.0625,0.125,0.25,0.5,1,2,4,8,16] ;
Yticks = 2.^(fix(log2(min(period))):fix(log2(max(period))));
contourf(time,log2(period),log2(power),log2(levels),'linestyle','none');  %*** or use 'contourfill'
%imagesc(time,log2(period),log2(power));  %*** uncomment for 'image' plot
xlabel('Year')
ylabel('Period (years)')

set(gca,'XLim',xlim(:))
set(gca,'YLim',log2([min(period),64]), ...
	'YDir','reverse', ...
	'YTick',log2(Yticks(:)), ...
	'YTickLabel',Yticks)
% 95% significance contour, levels at -99 (fake) and 1 (95% signif)
hold on
contour(time,log2(period),sig95,[-99,1],'k', 'linewidth', 2);
hold on
% cone-of-influence, anything "below" is dubious
plot(time,log2(coi),'k')
plot([cutoff_year cutoff_year], log2([min(scale) max(scale)]), '-', 'Color', 'black', 'linewidth', 3)
plot([cutoff_year cutoff_year], log2([min(scale) max(scale)]), '-', 'Color', 'white', 'linewidth', 1.5)
hold off;

gTextBold(gca, 'D', 0.05, 0.95);

%subplot('position',[0.05 0.45 0.4 0.5])
%--- Plot global wavelet spectrum
subplot('position',[0.46+bumpright 0.1 0.15 0.4])
plot(global_ws,log2(period), 'k', 'linewidth', 2)
hold on
plot(global_signif,log2(period),'--', 'color', [0.5 0.5 0.5])
hold off
xlabel('Power (day^2)')

set(gca,'YLim',log2([min(period),64]), ...
	'YDir','reverse', ...
	'YTick',log2(Yticks(:)), ...
	'YTickLabel','')
set(gca,'XLim',[0,1.25*max(global_ws)])
gTextBold(gca, 'F', 0.05, 0.95,'none');



subplot('position',[0.8+bumpleft 0.1 0.15 0.4]);
[vyears, vstd, vdiff] = calcVar(time, data);
plot(vyears, vstd, 'o-', 'linewidth', 2);
hold all;
plot(vyears, vdiff, 'o-', 'linewidth', 2);
set(gca, 'XLim',[1700 2010]);
ylabel('Variability (days)');
xlabel('Year')
legend({'SD', 'Mean-diff'});
gTextLeftBold(gca, 'B', 0.05, 0.05, 'none');