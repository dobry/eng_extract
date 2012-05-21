%% narysuj wszystkie sygnaly

clear all;
close all;



% Dir = 'extracted/skrzycka_ewa/';
Dir = 'extracted/szaniawska_chydzinska_jadwiga/';
% Dir = 'extracted/olczak_iwona/';
% Dir = 'extracted/lechkun_malgorzata/';
% Dir = 'extracted/hoppe_malgorzata/';
% Dir = 'extracted/domagala_krystyna/';

Sig = dir(Dir)
L = length(Sig)

for i = 3 : L
    
    fileID = fopen(strcat(Dir, Sig(i).name),'r');
    Int16 = fread(fileID, inf, 'int16');
    
    figure
    hold on;
    plot(Int16);
    title(Sig(i).name);
    hold off;
    
    fclose(fileID);
end

%% narysuj wybrany sygnal

fileID = fopen(strcat(Dir, 'signal10'),'r');
Int16 = fread(fileID, inf, 'int16'); % prawdopodobnie to jest to
figure
%subplot(9, 2, i - 1);
plot(Int16);
fclose(fileID);