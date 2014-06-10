
d = importdata('../Data/torn.allyears.tsv');

d.data(:,2) = detrend(d.data(:,2));

figure;
[wave,period,scale,coi,sig95] = wt(d.data(:,1:2));

wt(d.data(:,1:2));
export_fig('Figures/wt-all-torn.tiff','-r300');

%%

for i=1:size(wave,2)
    wave(scale > coi(i),i) = 0;
    sig95(scale > coi(i),i) = 0;
end

sig95 = sig95.*abs(wave);

plot(mean(abs(wave),2), scale, mean(sig95,2), scale)
set(gca,'YDir','reverse', 'yscale', 'log');
ylim([min(scale),max(scale)])

%%
X=d.data(:,2);

[P,freq]=pburg(zscore(X),10,[],1);
aa=ar1(X);
Ptheoretical=(1-aa.^2)./(abs(1-aa.*exp(-2*pi*i*freq))).^2;
semilogy(freq,P/sum(P),freq,Ptheoretical/sum(Ptheoretical),'r');
legend('observed',sprintf('Theoretical AR1=%.2f',aa),'location','best')


%%
indx = 7
d = importdata('../Torn1803.txt');
wt(d.data(:,[1 indx]));
title(d.colheaders{indx},'interpreter','none');
%export_fig('Figures/wt-NAO.tiff','-r300');


%% 
figure
plotyy(d.data(:,1), d.data(:,2), d.data(:,1), d.data(:,4))

