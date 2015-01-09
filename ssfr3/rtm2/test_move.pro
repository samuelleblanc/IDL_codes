pro test_move,in

outdir = '/data/seven/schmidt/attrex/gh/test/'
indir  = '/data/seven/schmidt/calnex/p3/20100516/'
action = ''
if n_elements(in) lt 1 then i=0 else i=in
while not strcmp(action,'s') do begin
  if i eq 101 then i=102 
  print, 'copying: '+string(i,format='(I05)')
  spawn, 'cp '+indir+'spc'+string(i,format='(I05)')+'.OSA2 '+outdir
  spawn, 'cp '+indir+'cg4'+string(i,format='(I05)')+'.CG4 '+outdir
  
  i=i+1
 ; if i lt 20 then wait,1 else wait, 30
  if i lt 10 then wait, 10 else wait,30
  action=get_kbrd(0) ; s=stop
endwhile


end
