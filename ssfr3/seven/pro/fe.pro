;function to estimate exponential fit

function fe, x, a

  return, a[0]+a[1]*exp((x)/a[2])
;return, a[0]+exp((x-a[1])/a[2])
end

function err_tau, x
  return, 0.02-0.0004*(x-100.)+0.0000085*(x-50.)^2.
end


