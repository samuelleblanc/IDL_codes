function cfg,file,search
on_error,2
n=file_lines(file)
openr,uc,file,/get_lun ; open configuration file
string=''              ; initialize string
for i=0,n-1 do begin
  readf,uc,string
  if strmid(string,0,1) ne '#' and strmid(string,0,1) ne ';' and strmid(string,0,1) ne ' ' then begin ; exclude comment lines
    pos=strpos(string,' ')
    item=strmid(string,0,pos)
    pos=strpos(strupcase(item),strupcase(search)) ; search item
    if pos ne -1 and strupcase(item) eq strupcase(search) then result=string
  endif
endfor
free_lun,uc
if n_elements(result) lt 1 then begin
  result='#'
  return,result
endif
pos=strpos(result,' ')
if pos lt 0 then message,'Error in cfg file.'
result=strmid(result,pos)
pos=strpos(result,'#')          ; cut off comendts at end of line
if pos gt 1 then result=strmid(result,0,pos)
result=strtrim(result,2)
return,result ; return string for searched item
end
