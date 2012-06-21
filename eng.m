%% 
clear all;
close all;

Path = 'extracted/';

% patients
% Dir = 'skrzycka_ewa';
% Dir = 'olczak_iwona';
% Dir = 'lechkun_malgorzata';
% Dir = 'hoppe_malgorzata';
Dir = 'domagala_krystyna';
% Dir = 'szaniawska_chydzinska_jadwiga';

%% save petient's signals to .mat files

MatDir = 'mat/';
mkdir(MatDir);

Sig = dir(strcat(Path, Dir));
L = length(Sig);

for i = 3 : L
    fileID = fopen(strcat([Path, Dir, '/', Sig(i).name]),'r');
     % dla szaniawskiej fread(fileID, inf, 'bit24'); !
     % sygnały szaniawskiej są zapisane jako signed integer 24-bit
    data = fread(fileID, inf, 'int16');

    genvarname([Sig(i).name(4:end)])
    patient.(genvarname([Sig(i).name(4:end)])) = data;

    fclose(fileID);
end
save(strcat([MatDir, Dir, '.mat']), 'patient');

%% plot patient's signals from binary files

Sig = dir(strcat(Path, Dir));
L = length(Sig);

for i = 3 : L
    
    fileID = fopen(strcat([Path, Dir, '/', Sig(i).name]),'r');
     % dla szaniawskiej fread(fileID, inf, 'bit24'); !
     % sygnały szaniawskiej są zapisane jako signed integer 24-bit    
    Int16 = fread(fileID, inf, 'int16');

    figure
    hold on;
    plot(Int16);
    title(Sig(i).name);
    hold off;
    
    fclose(fileID);
end
