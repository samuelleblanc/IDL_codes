;program that stores a few functions for curve fitting


pro logs, x, a,f,pder

f=a[0]+a[1]*alog(x+a[2])

if n_params() ge 4 then begin
pder = FLTARR(N_ELEMENTS(X), 3)  
      ; Compute the partial derivatives with respect to  
      ; a0 and place in the first row of PDER:  
      pder[*, 0] = 1.0   
      ; Compute the partial derivatives with respect to  
      ; a1 and place in the second row of PDER:  
      pder[*, 1] = alog(x+a[2])
      pder[*, 2] = a[1]/(x+a[2])
endif  
end

pro roots, x, a,f,pder

f= a[0]+a[1]*(x)^a[2]/2.

if n_params() ge 4 then begin
pder = FLTARR(N_ELEMENTS(X), 3)
      ; Compute the partial derivatives with respect to  
      ; a0 and place in the first row of PDER:  
      pder[*, 0] = 1.0
      ; Compute the partial derivatives with respect to  
      ; a1 and place in the second row of PDER:  
      pder[*, 1] = x^(a[2]/2.)
      pder[*, 2] = a[1]*a[2]/4.*x^(a[2]/2.)
endif

end

pro uexp, x, a,f,pder

f=a[0]/(a[1]-exp(x+a[2]))
if n_params() ge 4 then begin
      pder = FLTARR(N_ELEMENTS(X), 3)
      ; Compute the partial derivatives with respect to  
      ; a0 and place in the first row of PDER:  
      pder[*, 0] = 1.0/(a[1]-exp(x+a[2]))
      ; Compute the partial derivatives with respect to  
      ; a1 and place in the second row of PDER:  
      pder[*, 1] = 1.0/(a[1]-exp(x+a[2]))
      pder[*, 2] = -exp(x+a[2])*(x+a[2])/(a[1]-exp(x+a[2]))

endif

end

pro exps, x, a,f,pder

f=exp((x-a[1])/a[2])-a[0]
if n_params() ge 4 then begin
      pder = FLTARR(N_ELEMENTS(x), n_elements(a))
      ; Compute the partial derivatives with respect to  
      ; a0 and place in the first row of PDER:  
      pder[*, 0] = 1.0
      ; Compute the partial derivatives with respect to  
      ; a1 and place in the second row of PDER:  
      pder[*, 1] = exp((x-a[1])/a[2])*(x-a[1])/a[2]*(-1.0)
      pder[*, 2] = exp((x-a[1])/a[2])*(x-a[1])^2./a[2]
endif

end
