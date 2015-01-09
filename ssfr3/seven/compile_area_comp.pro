; program to put together all the different modeled area results
; restore each save file independently then put together in large save file

pro compile_area_comp

dir='/argus/SSFR3/model/'

restore, dir+'area_comp4_20120602.out'
mar1a=area & mar2a=area2 & mar3a=area3 & moxaa=oxa

restore, dir+'area_comp4_20120813.out'
mar1b=area & mar2b=area2 & mar3b=area3 & moxab=oxa

restore, dir+'area_comp4_20120525.out'
mar1c=area & mar2c=area2 & mar3c=area3 & moxac=oxa

restore, dir+'area_comp4_20120523.out'
mar1d=area & mar2d=area2 & mar3d=area3 & moxad=oxa

restore, dir+'area_comp4_20120912.out'
mar1e=area & mar2e=area2 & mar3e=area3 & moxae=oxa

restore, dir+'area_comp4_20120806.out'
mar1f=area & mar2f=area2 & mar3f=area3 & moxaf=oxa

restore, dir+'area_comp4_20120816.out'
mar1g=area & mar2g=area2 & mar3g=area3 & moxag=oxa

restore, dir+'area_comp4_20120820.out'
mar1h=area & mar2h=area2 & mar3h=area3 & moxah=oxa

restore, dir+'area_comp4_20120824.out'
mar1i=area & mar2i=area2 & mar3i=area3 & moxai=oxa

mtotarea=[mar1a,mar1b,mar1c,mar1d,mar1e,mar1f,mar1g,mar1h,mar1i]
mtotarea2=[mar2a,mar2b,mar2c,mar2d,mar2e,mar2f,mar2g,mar2h,mar2i]
mtotarea3=[mar3a,mar3b,mar3c,mar3d,mar3e,mar3f,mar3g,mar3h,mar3i]
mtotoxa=[moxaa,moxab,moxac,moxad,moxae,moxaf,moxag,moxah,moxai]

;restore, dir+'cloudy.out'

save, mtotarea,mtotarea2,mtotarea3,mtotoxa, filename=dir+'cloudy_model4.out'
stop

end
