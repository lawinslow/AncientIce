function [vyears, vstd, vdiff] = calcVar(years, doy)

stride = 30;

diffs = vertcat(abs(diff(doy)),0);
decades = floor((1:length(years))/stride)*stride+years(1);

vyears = unique(decades);
vstd = nan(length(vyears), 1);
vdiff = nan(length(vyears), 1);

for i=1:length(vyears)
    idx = decades == vyears(i);
    
    if(sum(isnan(doy(idx))) <= 13)
        tmp = doy(idx);
        tmp(isnan(tmp)) = 41;
        [~,sigmahat] = normfit(tmp, 0.05, tmp==41);
        %vstd(i) = std(tmp(~isnan(tmp)));
        vstd(i) = sigmahat;
    end
    
    if(sum(isnan(diffs(idx))) <= 10)
        tmp = diffs(idx);
        vdiff(i) = mean(tmp(~isnan(tmp)));
    end
    
end

vyears = vyears + (stride/2);

vstd   = vstd(vyears < max(years));
vdiff  = vdiff(vyears < max(years));
vyears = vyears(vyears < max(years));

end