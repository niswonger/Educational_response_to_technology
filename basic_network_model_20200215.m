clear all 
%% Set key parameters
global N_tot C sigma n m ed lambda w_l
N_tot = 10000; % total population
C = 2; % number of cities
f_s = .3; % initial high skill fraction
w_l = 15; % low skilled wage
w_h1 = 20; % high skilled wage in city 1
w_h2 = 25; % high skilled wage in city 2
sigma = 2; % variance in wage
n = .05; % level of churn in the labor market 
d_l = 2*[3,1;
       1,3]; % mean connections for low skilled from city to city
d_s = 2*[10,3;
       3, 10]; % mean connections for high skilled from city to city
m = 1.8; % Moving cost
ed = 5; % training cost 
lambda = 10; % objective function penalty for uncertainty 
%% Set initial population
% Assign cities to the population
c = randsample(repmat(1:C,1,N_tot/C),N_tot)';
% Assign skills to the population
s = rand(N_tot,1)>(1-f_s);
% Set initial mean wages
w_bar = [ones(1,C)*w_l;w_h1,w_h2 ]';
% Define initial wage vector
w = zeros(N_tot,1);
% Instantiate index for city skills
cs_index = zeros(N_tot,C,2);
% Instantiate count of each group 
N = zeros(size(w_bar));
% get wages for the initial population
for i = 1:C 
    for j = 1:2
        cs_index(1:N_tot,i,j) = c == i & s == j-1;  
        N(i,j) = sum(cs_index(1:N_tot,i,j));
        w(logical(cs_index(1:N_tot,i,j))) ...
            = normrnd(w_bar(i,j),sigma,N(i,j),1);
    end
end



%% Get next generation
% this function returns a new population of locations, skill levels, wages
% as well as showing how many in the last generation moved or chose to
% upgrade skills.   
[c_next,s_next,w_next,move,edu] = solveNextGen(d_l,d_s,c,s,w,w_bar); 

%% Now generate "new" people
% Get proportional new labor 
N_new = N_tot*n;
% Define mean connections for each skill level going from city i to city j
d = cat(3,d_l,d_s);
% Store mean observed wages 
w_obs = zeros(N_new,C);
d_obs = zeros(N_new,C);
c_new = zeros(N_new,1);

skill_new = zeros(N_new,1);
for j = 1:C 
    index = 1;
    for i = 1:C
        for k = 1:2
            index_new = round(n*N(i,k));
            % get the number of connections from a poisson draw
            d_samp = poissrnd(d(i,j,k),index_new,1);
            % Sample from the relevant wage to get the mean from the sample 
            fun = @(d)mean(randsample(w(logical(cs_index(:,j,2))),d));
            w_obs(index:index+index_new-1,j) = arrayfun(fun,d_samp);
            c_new(index:index+index_new-1) = i;
            d_obs(index:index+index_new-1,j) = d_samp;
            skill_new(index:index+index_new-1,1) = k;
            index = index + index_new;
        end
    end
end
w_obs(isnan(w_obs))=0;
% New people choose skills and location
U_new = w_obs-lambda./(d_obs).^.5-[m,0].*(c_new==2)-[0,m].*(c_new==1)-ed;
m_obs = (c_new == 2 & (U_new(:,1) == max(U_new,[],2)& max(U_new,[],2)>w_l))|...
    (c_new == 1 & (U_new(:,2) == max(U_new,[],2)& max(U_new,[],2)>w_l));
s_new = (max(U_new,[],2)>w_l);
sum(s_new)/N_new
sum(m_obs)/N_new
%% Determine the new population's wage, remove "old" people and merge
% Get new cities
c_new(c_new == 1 & m_obs ==1) = 2;
c_new(c_new == 2 & m_obs ==1) = 1;
w_new = zeros(N_new,1); % instantiate the wage
% Get wages for the new population
for i = 1:C 
    for j = 1:2
        w_new(c_new == i & s_new == j-1) ...
            = normrnd(w_bar(i,j),sigma,sum(c_new == i & s_new == j-1),1);
    end
end
% Now we eliminate the same fraction from the population as a whole and 
keepers = randsample(1:N_tot,.95*N_tot);
c_next = [c_new;c(keepers)];
s_next = [s_new;s(keepers)];
w_next = [w_new;w(keepers)];


mean(c_next)
mean(s_next)
mean(w_next)