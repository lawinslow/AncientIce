addpath('wtc');

d = importdata('../Torn1803.txt');

d.data(:,2:end) = detrend(d.data(:,2:end));


%% XWT 
figure();

one = 2;
two = 8;
xwt(d.data(:,[1 one]), d.data(:,[1 two]));

hmmm = get(gca,'xtick');
%set(gca,'xtick',[])
%set(gca,'ytick',[])
title([d.colheaders{one} ':' d.colheaders{two}],'interpreter','none');
export_fig('Figures/xwt-Torn-Mamtornio.tiff','-r300')

%%
figure;
plotyy(d.data(:,1), d.data(:,2),d.data(:,[1]), d.data(:,4))


%% 
figure('color',[1 1 1]);
xwt(d.data(:,[1 2]), d.data(:,[1 4]));
title(['XWT - ' d.colheaders{2} ':' d.colheaders{4}],'interpreter','none');
export_fig('Figures/xwt-Torn-Sunspot.tiff','-r300')

figure('color',[1 1 1]);
subplot(2,1,1);
wt(d.data(:,[1 2]));
ylabel(d.colheaders{2}, 'interpreter','none')


subplot(2,1,2);
wt(d.data(:,[1 4]));
ylabel(d.colheaders{4}, 'interpreter','none')
export_fig('Figures/wt-Torn-Sunspot.tiff','-r300')


