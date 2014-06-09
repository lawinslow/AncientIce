%% import data
addpath('wtc');

d = importdata('../Torn/torn.allyears.tsv');
nao = importdata('../AllData/noa.long.tsv');

uyears = unique(nao.data(:,1));
unao = nan(length(uyears),1);
for i=1:length(uyears)
    uI = uyears(i) == nao.data(:,1);
    
    unao(i) = mean(nao.data(uI,2));
end

nao_fix = [uyears unao];

%% Mean NAO different from monthly?
figure;
subplot(1,2,1);

x = linspace(min(nao.data(:,1)), max(nao.data(:,1)), length(nao.data));
wt([x' nao.data(:,2)])
title('monthly nao')

subplot(2,2,4);
wt(nao_fix);

export_fig('Figures/NAO_monthly_yearly.tiff','-r300');



%% Cross Wavelet of TORN and NAO
figure;

xwt(d.data, nao_fix);
title('up:ice leading NAO by 90deg')

export_fig('Figures/xwt-Torn-NAO-long.tiff','-r300');