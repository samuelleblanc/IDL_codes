;program that stores a few functions for curve fitting


pro logs, x, a,f,pder

f=a[0]+a[1]*alog(x+a[2])

if n_params() ge 4 then $
pder = FLTARR(N_ELEMENTS(X), 3)  
      ; Compute the partial derivatives with respect to  
      ; a0 and place in the first row of PDER:  
      pder[*, 0] = 1.0   
      ; Compute the partial derivatives with respect to  
      ; a1 and place in the second row of PDER:  
      pder[*, 1] = alog(x+a[2])
      pder[*, 2] = a[1]/(x+a[2])  
end

function roots, x, a,b,c

return, a+b*(x)^c/2.

end

function uexp, x, a,b,c

return, a/(b-exp(x+c))
end
