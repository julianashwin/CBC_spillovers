% Define the parameter values

sigeps = 1^2;
signu = 0.4^2;
sigeta = 0.2^2;

% Define the shocks

s_1 = 3.0;
s_2 = 1;

lambda1 = @(a) sigeps./(sigeps + signu + (1-a).^2.*sigeta);
lambda2 = @(a) sigeps./(sigeps + signu + a.^2.*sigeta);

dlambda1da = @(a) 2.*(1-a).*lambda1(a).^2.*sigeta./sigeps;
dlambda2da = @(a) -2.*a.*lambda2(a).^2.*sigeta./sigeps;


L = @(a,s_1) ((lambda1(a) - 1).^2.*s_1.^2 + lambda1(a).^2.*(1-a).^2.*sigeta ...
    + 2.*signu + (lambda2(a) - 1).^2.*s_2.^2 + lambda2(a).^2.*a.^2*sigeta);

dLda = @(a,s_1) s_1.*2.*(lambda1(a) -1).*dlambda1da(a) - lambda1(a).^2.*sigeta ... 
    + 2.*lambda1(a).*dlambda1da(a).*(1-a).*sigeta ...
    + s_2.*2.*(lambda2(a) -1).*dlambda2da(a) + lambda2(a).^2.*sigeta ... 
    + 2.*lambda2(a).*dlambda2da(a).*a.*sigeta;

%x = 0:0.01:1;

%Lx = L(x);

%plot(x, Lx)

na=50;
ns=100;
as=zeros(1,na);
for i=1:na
    as(i)=(i-1)/(na-1);
end
S1s=zeros(1,ns);
for i=1:ns
    S1s(i)=1+(10/ns)*(i-1);
end

Lmat=zeros(na,ns);
dLdamat=zeros(na,ns);
for i=1:na
    for j=1:ns
        s_1=S1s(j);
        Lmat(i,j)=L(as(i),s_1);
        dLdamat(i,j)=dLda(as(i),s_1);
    end
end

figure(1);
surfc(S1s,as,Lmat);
xlabel('s_1');
ylabel('a_1');
zlabel('L');


figure(2);
contour(S1s,as,dLdamat,[0,0]);
xlabel('s_1');
ylabel('a_1');
zlabel('L');
    