;program to test io properties
pro testio

for i=0, 10 do begin
on_ioerror,out
f='test.txt'
openr,lun, f, /get_lun


out:begin
print, 'not opened',i

 continue
end
print, 'opened',i

close,lun
endfor

end
