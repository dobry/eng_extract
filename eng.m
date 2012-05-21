%% narysuj wszystkie sygnaly

clear all;
close all;



Dir = 'extracted/';
Sig = dir(Dir)
L = length(Sig)

for i = 3 : L
    
    fileID = fopen(strcat(Dir, Sig(i).name),'r');
    Int16 = fread(fileID, inf, 'int16'); % prawdopodobnie to jest to
%     frewind(fileID);
%     Int32 = fread(fileID, inf, 'int32');
    
    figure
       
%     subplot(2, 1, 1);
    hold on;
    plot(Int16);
    title(Sig(i).name);
    hold off;
    
%     subplot(2, 1, 2);
%     hold on;
%     plot(Int32);
%     title(Sig(i).name);
%     hold off;
    
    fclose(fileID);
end

%% narysuj wybrany sygnal

fileID = fopen(strcat(Dir, 'signal10'),'r');
Int16 = fread(fileID, inf, 'int16'); % prawdopodobnie to jest to
figure
%subplot(9, 2, i - 1);
plot(Int16);
fclose(fileID);