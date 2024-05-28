function [ICs] = compareModels(File)

datasets = datastruct (File);

model1=StandardMixtureModel();
model2=SwapModel();
for i = 1:size(datasets,1)
    for j = 1:size(datasets,2)
       % I have made some change to make it automatically in MemFit
%                 %   if verbosity > 0
%                 %     r = input(['Would you like to compute the DIC (note that this can be slow,\n' ...
%                 %       'since it requires running MCMC on each model)? (y/n): '], 's');
%                 %     fprintf('\n');
%                 %   end
% 
%                   if verbosity == 0
%                     r = 'y';  % compute dic and bayes factors
%                   end
% 
%                  if verbosity == 10
%                     r = 'n';  % NOT compute dic and bayes factors
%                  end
       ICs{i,j} = MemFit(datasets{i,j},{model1,model2},'Verbosity', 10);
       % ICs{i,j} = MemFit(datasets{i,j},{model1,model2}); % with the default package, you need to inout mannually
       % DICs{i,j} = MemFit(datasets{i,j},{model1,model2},'Verbosity', 0);
    end
end 

for i = 1:size(ICs,1)
    rName(i) = datasets{i,1}.condition + datasets{i,1}.n';
    for j = 1:size(ICs,2)
       dAIC{i,j} = ICs{i,j}.AIC-min(ICs{i,j}.AIC);
       dAIC_M1_M2(i,j) = ICs{i,j}.AIC (1)- ICs{i,j}.AIC(2);
       if ICs{i,j}.AIC (1) <= ICs{i,j}.AIC (2)
           aM1(i,j)=1;
       else 
           aM1(i,j)=0;
       end
       dBIC{i,j} = ICs{i,j}.BIC-min(ICs{i,j}.BIC);    
       dBIC_M1_M2(i,j) = ICs{i,j}.BIC (1)- ICs{i,j}.BIC(2);
       if ICs{i,j}.BIC (1) <= ICs{i,j}.BIC (2)
           bM1(i,j)=1;
       else
           bM1(i,j)=0;
       end
    end
end
save ICs.mat

% save some results for ploting 
cName="par"+ (1:size(ICs,2));
T_dAIC_M1_M2 = array2table(dAIC_M1_M2,'RowNames',rName','VariableNames',cName);
T_dBIC_M1_M2 = array2table(dBIC_M1_M2,'RowNames',rName','VariableNames',cName);
modelcomparisionFolder = pwd;
savepath = [modelcomparisionFolder, '/','outputICS'];
writetable(T_dAIC_M1_M2,[savepath,'/','dAIC_M1_M2.csv'],'WriteRowNames',true);
writetable(T_dBIC_M1_M2 ,[savepath,'/','dBIC_M1_M2.csv'],'WriteRowNames',true);

% in each condition, which model is facvoured?
eachCondFit(:,1) = sum(aM1,2)/size(aM1,2);
eachCondFit(:,2) = sum(bM1,2)/size(bM1,2);
eachCondFit(size(aM1,1)+1,1) = sum(sum(aM1))/(size(aM1,1)*size(aM1,2));
eachCondFit(size(aM1,1)+1,2) = sum(sum(bM1))/(size(bM1,1)*size(bM1,2));
RowNames = [rName,'All'];
VariableNames = {'AIC' 'BIC'};
T_eachCondFit = array2table(eachCondFit,'RowNames',RowNames','VariableNames',VariableNames);
 writetable(T_eachCondFit,[savepath,'/','eachCondFit.csv'],'WriteRowNames',true);
end

