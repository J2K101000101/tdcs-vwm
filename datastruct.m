function [datasets] = datastruct(File)
% data struct array for MemFit (tDCS data struct)

opts = delimitedTextImportOptions("NumVariables", 28);

% Specify range and delimiter
opts.DataLines = [2, Inf];
opts.Delimiter = ",";

% Specify column names and types
opts.VariableNames = ["VarName1", "extId", "session", "sessionId", "userCode", "moduleId", "sessionToken", "trialId", "executableId", "sessioncomplete", "sessioncondition", "sessionforceExit", "setSize", "stimulusValue1", "stimulusValue2", "stimulusValue3", "stimulusValue4", "stimulusValue5", "stimulusValue6", "probeItem", "probeAngle", "reactionTime", "givenResponse", "deviation", "deviationAbs", "sessionstartTime", "sessionendTime", "condition"];
opts.VariableTypes = ["double", "double", "double", "double", "double", "categorical", "double", "double", "double", "double", "categorical", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "categorical", "categorical", "string"];

% Specify file level properties
opts.ExtraColumnsRule = "ignore";
opts.EmptyLineRule = "read";

% Specify variable properties
opts = setvaropts(opts, ["moduleId", "sessioncondition", "sessionstartTime", "sessionendTime", "condition"], "EmptyFieldRule", "auto");
opts = setvaropts(opts, ["sessionToken", "executableId", "stimulusValue5", "stimulusValue6"], "TrimNonNumeric", true);
opts = setvaropts(opts, ["sessionToken", "executableId", "stimulusValue5", "stimulusValue6"], "ThousandsSeparator", ",");


%data = readtable(File.name,'ReadRowNames',true,'ReadVariableNames',true,'VariableNamingRule','preserve');
data = readtable([File.folder, '/',File.name],opts);
ID = data.extId;
for i = 1:size(unique(ID),1) % participant
    id = 100+i;
    t1 = data(data.extId == id,:);
    %Sham_ss2
    t2{1} = t1(strcmp(t1.condition, 'Sham') & t1.setSize == 2,:);
    %Sham_ss4
    t2{2} = t1(strcmp(t1.condition, 'Sham') & t1.setSize == 4,:);
    %Sham_ss6
    t2{3} = t1(strcmp(t1.condition, 'Sham') & t1.setSize == 6,:);
    %PPC_ss2
    t2{4} = t1(strcmp(t1.condition, 'PPC') & t1.setSize == 2,:);
    %PPC_ss4
    t2{5}= t1(strcmp(t1.condition, 'PPC') & t1.setSize == 4,:);
    %PPC_ss6
    t2{6}= t1(strcmp(t1.condition, 'PPC') & t1.setSize == 6,:);
    %DLPFC_ss2
    t2{7}= t1(strcmp(t1.condition, 'DLPFC') & t1.setSize == 2,:);
    %DLPFC_ss4
    t2{8}= t1(strcmp(t1.condition, 'DLPFC') & t1.setSize == 4,:);
    %DLPFC_ss6
    t2{9}= t1(strcmp(t1.condition, 'DLPFC') & t1.setSize == 6,:);
     
     
     for j =1:9 
           datasets{j, i}.errors = t2{1, j}.deviation';
           target = t2{1, j}.probeAngle;
           probe = t2{1, j}.probeItem;
           setsize = unique(t2{1, j}.setSize);
           stimuli = table2array(t2{1, j}(:,14:14+setsize-1));
           dev = stimuli - target;
           Dev = mod(dev+180,360)-180;
           for k = 1:size(Dev,1)
           Dev(k,probe(k))= NaN;
           end
           clear dis; 
           for h = 1:size(Dev,1)
           dis(h,:) = rmmissing (Dev(h,:));
           distractors = dis';
           end
           datasets{j, i}.distractors = distractors;
           datasets{j, i}.n = setsize;
           datasets{j, i}.subjectID = unique(t2{1, j}.extId);
           datasets{j, i}.condition = unique(t2{1, j}.condition);
     end
           
end

end

