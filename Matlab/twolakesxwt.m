
torn = importdata('../torn.allyears.tsv');

suwa = importdata('../suwa_late.tsv');

torn.data = torn.data(ismember(torn.data(:,1), suwa.data(:,1)),:);


plot(torn.data(:,1), torn.data(:,2), suwa.data(:,1), suwa.data(:,3));

xwt(torn.data(:,1:2), suwa.data(:, [1 3]));
export_fig('Figures/xwt-suwa-torn.tiff','-r300');