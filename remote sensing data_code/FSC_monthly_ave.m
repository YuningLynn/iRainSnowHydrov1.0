clc;clear
file_path = 'D:\常用数据\HMA_MODIS_FSC_2002\12\';% 图像文件夹路径 
img_path_list = dir(strcat(file_path,'*.tif'));%获取该文件夹中所有TIF格式的图像 
img_num = length(img_path_list);%获取图像总数量
II=cell(1,img_num);
[mask,Q]=geotiffread('D:\常用数据\两江流域水文气象水库数据集\cj\TP_basin_boundary\jsj500mshp.tif'); 
mask=double(mask); %[25.233387767944000,35.768387767944000][90.537345767791000,1.027023457677910e+02] 金沙江范围
mask(mask==0)=nan;
%% 读取逐日积雪覆盖度
sfc_month = [];
for jj = 1:img_num
    image_name = img_path_list(jj).name;
    [image, geo] = geotiffread(fullfile(file_path, image_name));
    II{jj} = image;
    sfc = image;
    sfc(sfc > 100) = 0;
    % 确保 sfc 的大小适应研究区域
    sfc_part1 = sfc(2046:3932, 5708:8140);
    sfc_part2 = zeros(220, 2433);
    sfc_jsj = [sfc_part1; sfc_part2];
    sfc_month = [sfc_month, sfc_jsj];
    fprintf(' %d %s\n', jj, fullfile(file_path, image_name));
end
%% 将日结果处理为月平均
A=reshape(sfc_month,[2107,2433,img_num]);
average_result = mean(A, 3);
SFC_jsj= average_result.*mask;
%SFC_jsj(isnan(SFC_jsj)) = -9999;  
h=imagesc(SFC_jsj);
set(h,'alphadata',~isnan(SFC_jsj));
maxTif=max(max(SFC_jsj));  
%image(find(SFC_jsj<0))=[];
minTif=min(min(SFC_jsj)); 
meanTif=nanmean(SFC_jsj,'all'); 
fprintf(' %d %f %f',maxTif,minTif,meanTif);
output_file = 'D:\常用数据\HMA_MODIS_FSC_2017\sfcave2002_12.tif';  
geotiffwrite(output_file, SFC_jsj, Q);  % 保存为 TIF 文件
clc;clear
folderPath = 'D:\常用数据\HMA_MODIS_FSC_2017\';
[mask,Q]=geotiffread('D:\常用数据\两江流域水文气象水库数据集\cj\TP_basin_boundary\jsj500mshp.tif'); 
mask=double(mask); %[25.233387767944000,35.768387767944000][90.537345767791000,1.027023457677910e+02] 
mask(mask==0)=nan;
matrix3D = zeros(size(mask,1), size(mask,2), 12);
for i = 1:12
    matrix3D(:, :, i) = mask;
end
for year = 2002:2018
    for month = 1:12
        monthStr = num2str(month); % 将月份转换为字符串，例如 '1'、'2'、'3'
        fileName = fullfile(folderPath, ['sfcave', num2str(year), '_', monthStr, '.tif']);
        if exist(fileName, 'file')
            data = imread(fileName);
            matrix3D(:, :, month) = matrix3D(:, :, month) + double(data);
        end
    end
end
c=colorbar;
caxis([0,100])
A=1/16*matrix3D(:, :, 1);
maxTif = max(A, [], 'all');  % 最大值
minTif = min(A, [], 'all');  % 最小值
meanTif = nanmean(A, 'all'); % 平均值
h=image(A);
set(h,'alphadata',~isnan(A));
fprintf('Max: %f, Min: %f, Mean: %f\n', maxTif, minTif, meanTif);
output_file = 'F:\graduate study\thesis\RMTS\subbasin\monthlysfc\1.tif';  
geotiffwrite(output_file, A, Q);  
