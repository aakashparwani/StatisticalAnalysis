clear;

%Below is the function, for that we are finding optimal maximum value.
%change the equation according to the requirement.
function y=fx1(X, N)
  y = sin(X(1)^2 + X(2)^2);
end

%definition of bfgs function
function [x_min,f_min,iters_min] = bfgs_min(N, X, gradient_toler, function_toler, delta_toler, 
  max_iter, myfunction)
% Function bfgs performs multivariate optimization using the
% Broyden-Fletcher-Goldfarb-Shanno method.
%
% Input
%
% N - number of variables
% X - array of initial guesses
% gradient_toler - tolerance for the norm of the slopes
% function_toler - tolerance for function
% delta_toler - array of delta X tolerances
% max_iter - maximum number of iterations
% myfunction - name of the optimized function
%
% Output
%
% X - array of optimized variables
% F - function value at optimum
% Iters - number of iterations
%
  
  %generate an identity matrix.
  B = eye(N,N);
  
  bGoOn = true;
  Iters = 0;
  
  % calculate initial gradient
  grad1 =  derivatives(X, N, myfunction);
   
  grad1 = grad1';%transpose
  
  %this is the loop where all the operations like search direction, step size etc.
  %calculations are performed.
  while bGoOn

    Iters = Iters + 1;
    
    fprintf('-----ITERATION - Minimum value search of variables----------%i.\n',Iters);
    
    if Iters > max_iter
      break;
    end
    
    %search direction to find minimum value of function
    S = -1 * B * grad1;
    S = S' / norm(S); % normalize vector S
    
    %initial value of step size
    lambda = 0.5;
    
    %perform line search to get value of lambda
    lambda = linsearch(X, N, lambda, S, myfunction);
    
    % calculate optimum X() with the given Lambda
    d = lambda * S;
    X = X + d;
    
    % get new gradient
    grad2 =  derivatives(X, N, myfunction);
    grad2 = grad2';
    
    g = grad2 - grad1;
    
    %update gradient value
    grad1 = grad2;
    
    fprintf('\n-----Minimum value of variables so far-------%i.\n',X);
    X
    
    % test for convergence
    for i = 1:N
      if abs(d(i)) > delta_toler(i)
        break
      end
    end

    if norm(grad1) < gradient_toler
      break
    end

    %in below steps we are performing bfgs update
    d = d';
    x1 = d * d';
    x2 = d' * g;
    x3 = d * g';
    x4 = g * d';
    x5 = g' * B * g;
    x6 = d * g' * B;
    x7 = B * g * d';
    update = (1 + x5 / x2) * x1 / x2 - x6 / x2 - x7 / x2;
    B = B + update;
    % break
  end
  
  %find optimal value of function with final value of variables.
  F = feval(myfunction, X, N);
  f_min = F;
  x_min = X;
  iters_min = Iters;

end

%
function y = myFxEx(N, X, DeltaX, lambda, myfunction)
% Function first calculate the new value of variables according to the
% updated value of lambda provided. And then, it calculate value of multivariate 
% equation that will be used to decide the correct line search. 

% Input
%
% N - number of variables
% X - array of variable values.
% DeltaX
% myfunction - name of the optimized function
%
% Output
%
% optimal solution for multivariate equation with final lambda.
%
  X = X + lambda * DeltaX;
  y = feval(myfunction, X, N);

end

%function to perform derivative.
function FirstDerivX = derivatives(X, N, myfunction)
  for iVar=1:N
    xt = X(iVar);
    h = 0.01 * (1 + abs(xt));
    X(iVar) = xt + h;
    
    fp = feval(myfunction, X, N);
    X(iVar) = xt - h;
    
    fm = feval(myfunction, X, N);
    X(iVar) = xt;
    
    FirstDerivX(iVar) = (fp - fm) / 2 / h;
  end
end

%function to perform linesearch.
function lambda = linsearch(X, N, lambda, D, myfunction)

  MaxIt = 100;
  Toler = 0.000001;
  
  fprintf('--Let us check what is going on in linesearch----%i.\n');
  
  iter = 0;
  bGoOn = true;
  while bGoOn
    iter = iter + 1;
    fprintf('------------ITERATION--------------%i.\n',iter);
    if iter > MaxIt
      lambda = 0;
      break
    end

    h = 0.01 * (1 + abs(lambda));
    f0 = myFxEx(N, X, D, lambda, myfunction);
    fprintf('------------Value of Lambda so far--------------%i.\n',lambda);
    
    fprintf('------------Value of function--------------%i.\n',f0);
    fp = myFxEx(N, X, D, lambda+h, myfunction);
    fm = myFxEx(N, X, D, lambda-h, myfunction);
    deriv1 = (fp - fm) / 2 / h;
    deriv2 = (fp - 2 * f0 + fm) / h ^ 2;
    if deriv2 == 0
      break
    end
    diff = deriv1 / deriv2;
    lambda = lambda - diff;
    if abs(diff) < Toler
      bGoOn = false;
      fprintf('------------Closing Line Search--------------%i.\n');
    end
  end
end
% end


%call bfgs function.
[x_min,f_min,iters_min] = bfgs_min(2, [3 3], 1e-7, 1e-7, [1e-5 1e-5], 100, 'fx1')

fprintf('------- Let us check out the final values--------------%i.\n');

fprintf("\n----Minimum value of variables-----%i.  \n",x_min);

fprintf("-----Minimum value of function is-----%i.  \n",f_min);

fprintf("-----Number of iterations to find minimum value of function-----%i.  \n",iters_min);
