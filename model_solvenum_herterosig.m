% Define the parameter values

p.sigeps1 = sqrt(1)^2; % variance of shock over state variable one
p.signu1 = sqrt(1)^2; % variance of shock to CB signal one
p.sigeta1 = sqrt(1)^2; % variance of communication noise for signal one
p.sigeps2 = sqrt(1)^2;
p.signu2 = sqrt(1)^2;
p.sigeta2 = sqrt(1)^2;



% Define the shocks

p.s_1 = 1;
p.s_2 = 1;
p.a = 0.5;

pinit = p;

lambda1 = @(p) p.sigeps1./(p.sigeps1 + p.signu1 + (1-p.a).^2.*p.sigeta1);
lambda2 = @(p) p.sigeps2./(p.sigeps2 + p.signu2 + p.a.^2.*p.sigeta2);

dlambda1da = @(p) 2.*(1-p.a).*lambda1(p).^2.*p.sigeta1./p.sigeps1;
dlambda2da = @(p) -2.*p.a.*lambda2(p).^2.*p.sigeta2./p.sigeps2;


L = @(p) ((lambda1(p) - 1).^2.*p.s_1.^2 + lambda1(p).^2.*(1-p.a).^2.*p.sigeta1 ...
    + p.signu1 +p.signu2 + (lambda2(p) - 1).^2.*p.s_2.^2 + lambda2(p).^2.*p.a.^2*p.sigeta2);

dLda = @(p) p.s_1.^2.*2.*(lambda1(p) -1).*dlambda1da(p) - 2.*lambda1(p).^2.*(1-p.a).*p.sigeta1 ... 
    + 2.*lambda1(p).*dlambda1da(p).*(1-p.a).^2.*p.sigeta1 ...
    + p.s_2.^2.*2.*(lambda2(p) -1).*dlambda2da(p) + 2.*lambda2(p).^2.*p.a.*p.sigeta2 ... 
    + 2.*lambda2(p).*dlambda2da(p).*p.a.^2.*p.sigeta2;

% Special case 
p.s_1 = 1;
p.s_2 = 1;
p.a = 0.5;

L(p)
dLda(p)

%x = 0:0.01:1;

%Lx = L(x);

%plot(x, Lx)

%% Plots


na=100;
as=zeros(1,na);
for i=1:na
    as(i)=(i-1)/(na-1);
end

% How does a change with s1?
p = pinit;

p.s_1 = -2.5; 
ns=101;
S1s=zeros(1,ns);
for i=1:ns
    S1s(i)=p.s_1+0.05*(i-1);
end
Lmats1=zeros(na,ns);
dLdamat_s1=zeros(na,ns);
for i=1:na
    for j=1:ns
        p.a = as(i);
        p.s_1 = S1s(j);
        Lmats1(i,j)=L(p);
        dLdamat_s1(i,j)=dLda(p);
    end
end

% Reset p
p = pinit;

p.signu1 = sqrt(0)^2; 
% How does a change with signu1?
nsignu1=101;
signu1s=zeros(1,nsignu1);
for i=1:nsignu1
    signu1s(i)=p.signu1+0.02*(i-1);
end
Lmatnu1=zeros(na,nsignu1);
dLdamat_nu1=zeros(na,nsignu1);
for i=1:na
    for j=1:nsignu1
        p.a = as(i);
        p.signu1 = signu1s(j);
        Lmatnu1(i,j)=L(p);
        dLdamat_nu1(i,j)=dLda(p);
    end
end


% Reset p
p = pinit;

p.sigeta1 = sqrt(0)^2; 
% How does a change with sigeta1?
nsigeta1=101;
sigeta1s=zeros(1,nsigeta1);
for i=1:nsigeta1
    sigeta1s(i)=p.sigeta1+0.02*(i-1);
end
Lmateta1=zeros(na,nsigeta1);
dLdamat_eta1=zeros(na,nsigeta1);
for i=1:na
    for j=1:nsigeta1
        p.a = as(i);
        p.sigeta1 = sigeta1s(j);
        Lmateta1(i,j)=L(p);
        dLdamat_eta1(i,j)=dLda(p);
    end
end


% Reset p
p = pinit;

p.sigeps1 = sqrt(0)^2; 
% How does a change with sigeps1?
nsigeps1=101;
sigeps1s=zeros(1,nsigeps1);
for i=1:nsigeps1
    sigeps1s(i)=p.sigeps1+0.02*(i-1);
end
Lmateps1=zeros(na,nsigeps1);
dLdamat_eps1=zeros(na,nsigeps1);
for i=1:na
    for j=1:nsigeps1
        p.a = as(i);
        p.sigeps1 = sigeps1s(j);
        Lmateps1(i,j)=L(p);
        dLdamat_eps1(i,j)=dLda(p);
    end
end




figure(1);
surfc(S1s,as,Lmats1);
xlabel('s_{1,t}');
ylabel('a_{1,t}');
zlabel('L');
title('Loss function, varying s_{1,t} and a_{1,t}')
saveas(gcf,'Lossfunction_s1.png')

figure(2);
contour(S1s,as,dLdamat_s1,[0,0]);
xlabel('s_{1,t}');
ylabel('a_{1,t}');
title('a_{1,t} which minimises L(a_{t}) for values of s_{1,t}')
saveas(gcf,'Solution_s1.png')


figure(3);
surfc(signu1s,as,Lmatnu1);
xlabel('\sigma^2_{\nu,1}');
ylabel('a_{1,t}');
zlabel('L');
title('Loss function, varying \sigma_{\nu,1} and a_{1,t}')
saveas(gcf,'Lossfunction_signu1.png')

figure(4);
contour(signu1s,as,dLdamat_nu1,[0,0]);
xlabel('\sigma^2_{\nu,1}');
ylabel('a_{1,t}');
zlabel('L');
title('a_{1,t} which minimises L(a_{t}) for values of \sigma_{\nu,1}')
saveas(gcf,'Solution_signu1.png')


figure(5);
surfc(sigeta1s,as,Lmateta1);
xlabel('\sigma^2_{\eta,1}');
ylabel('a_{1,t}');
zlabel('L');
title('Loss function, varying \sigma_{\eta,1} and a_{1,t}')
saveas(gcf,'Lossfunction_sigeta1.png')

figure(6);
contour(sigeta1s,as,dLdamat_eta1,[0,0]);
xlabel('\sigma^2_{\eta,1}');
ylabel('a_{1,t}');
zlabel('L');
title('a_{1,t} which minimises L(a_{t}) for values of \sigma_{\eta,1}')
saveas(gcf,'Solution_sigeta1.png')



figure(7);
surfc(sigeps1s,as,Lmateps1);
xlabel('\sigma^2_{\epsilon,1}');
ylabel('a_{1,t}');
zlabel('L');
title('Loss function, varying \sigma_{\epsilon,1} and a_{1,t}')
saveas(gcf,'Lossfunction_sigeps1.png')

figure(8);
contour(sigeps1s,as,dLdamat_eps1,[0,0]);
xlabel('\sigma^2_{\epsilon,1}');
ylabel('a_{1,t}');
zlabel('L');
title('a_{1,t} which minimises L(a_{t}) for values of \sigma_{\epsilon,1}')
saveas(gcf,'Solution_sigeps1.png')
    