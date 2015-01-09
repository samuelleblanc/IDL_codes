;function for extimate of the exponential fit

function expsf, x, a

return, exp((x-a[1])/a[2])-a[0]


end

