function [] = getParameters_tDCS(File,outputFolder,model)

            datasets=datastruct(File);
%             for k = 1:size(datasets,2)
%             rnames(1,k) = datasets{1, k}.subjectID;
%             end


%   if type == 1 % swap model
%             for i = 1:9
%             parameters = FitMultipleSubjects_MLE(datasets(i,:), model);
% 
%             g = parameters.paramsSubs(:,1);
%             B = parameters.paramsSubs(:,2);
%             sd = parameters.paramsSubs(:,3);
%             
%             switch i
%                 case 1
%                writematrix (g, [outputFolder,'/g_Sham_2.csv']);
%                writematrix (B, [outputFolder,'/B_Sham_2.csv']);
%                writematrix (sd, [outputFolder,'/sd_Sham_2.csv']);
%                 case 2
%                writematrix (g, [outputFolder,'/g_Sham_4.csv']);
%                writematrix (B, [outputFolder,'/B_Sham_4.csv']);
%                writematrix (sd, [outputFolder,'/sd_Sham_4.csv']);
%                 case 3
%                writematrix (g, [outputFolder,'/g_Sham_6.csv']);
%                writematrix (B, [outputFolder,'/B_Sham_6.csv']);
%                writematrix (sd, [outputFolder,'/sd_Sham_6.csv']);
%                   case 4
%                writematrix (g, [outputFolder,'/g_PPC_2.csv']);
%                writematrix (B, [outputFolder,'/B_PPC_2.csv']);
%                writematrix (sd, [outputFolder,'/sd_PPC_2.csv']);
%                 case 5
%                writematrix (g, [outputFolder,'/g_PPC_4.csv']);
%                writematrix (B, [outputFolder,'/B_PPC_4.csv']);
%                writematrix (sd, [outputFolder,'/sd_PPC_4.csv']);
%                 case 6
%                writematrix (g, [outputFolder,'/g_PPC_6.csv']);
%                writematrix (B, [outputFolder,'/B_PPC_6.csv']);
%                writematrix (sd, [outputFolder,'/sd_PPC_6.csv']);
%                 case 7
%                writematrix (g, [outputFolder,'/g_DLPFC_2.csv']);
%                writematrix (B, [outputFolder,'/B_DLPFC_2.csv']);
%                writematrix (sd, [outputFolder,'/sd_DLPFC_2.csv']);
%                 case 8
%                writematrix (g, [outputFolder,'/g_DLPFC_4.csv']);
%                writematrix (B, [outputFolder,'/B_DLPFC_4.csv']);
%                writematrix (sd, [outputFolder,'/sd_DLPFC_4.csv']);
%                 case 9
%                writematrix (g, [outputFolder,'/g_DLPFC_6.csv']);
%                writematrix (B, [outputFolder,'/B_DLPFC_6.csv']);
%                writematrix (sd, [outputFolder,'/sd_DLPFC_6.csv']);
%             end
%             end
% 
%    elseif type == 2 % standard mixture model
        
           for i = 1:9
            parameters = FitMultipleSubjects_MLE(datasets(i,:), model);

            G = parameters.paramsSubs(:,1);
            sd = parameters.paramsSubs(:,2);
            for j = 1: size(parameters.paramsSubs,1)
            precision(j) = inv(sd(j));
            end
       
            
            switch i
                case 1
                    setsize = 2;
                    capacity = (1-G)*setsize;
               writematrix (capacity, [outputFolder,'/capacity_Sham_2.csv']);
               writematrix (precision', [outputFolder,'/precision_Sham_2.csv']);
            
                case 2
                      setsize = 4;
                      capacity = (1-G)*setsize;
               writematrix (capacity, [outputFolder,'/capacity_Sham_4.csv']);
               writematrix (precision', [outputFolder,'/precision_Sham_4.csv']);
           
                case 3
                     setsize = 6;
                      capacity = (1-G)*setsize;
               writematrix (capacity, [outputFolder,'/capacity_Sham_6.csv']);
               writematrix (precision', [outputFolder,'/precision_Sham_6.csv']);
           
                  case 4
                    setsize = 2;
                    capacity = (1-G)*setsize;
               writematrix (capacity, [outputFolder,'/capacity_PPC_2.csv']);
               writematrix (precision', [outputFolder,'/precision_PPC_2.csv']);
                case 5
                    setsize = 4;
                    capacity = (1-G)*setsize;
               writematrix (capacity, [outputFolder,'/capacity_PPC_4.csv']);
               writematrix (precision', [outputFolder,'/precision_PPC_4.csv']);
                case 6
                    setsize = 6;
                    capacity = (1-G)*setsize;
               writematrix (capacity, [outputFolder,'/capacity_PPC_6.csv']);
               writematrix (precision', [outputFolder,'/precision_PPC_6.csv']);

                case 7
                    setsize = 2;
                    capacity = (1-G)*setsize;
               writematrix (capacity, [outputFolder,'/capacity_DLPFC_2.csv']);
               writematrix (precision', [outputFolder,'/precision_DLPFC_2.csv']);
                case 8
                    setsize = 4;
                    capacity = (1-G)*setsize;
               writematrix (capacity, [outputFolder,'/capacity_DLPFC_4.csv']);
               writematrix (precision', [outputFolder,'/precision_DLPFC_4.csv']);
                case 9
                    setsize = 6;
                    capacity = (1-G)*setsize;
               writematrix (capacity, [outputFolder,'/capacity_DLPFC_6.csv']);
               writematrix (precision', [outputFolder,'/precision_DLPFC_6.csv']);
            end

            end

%   end


end

