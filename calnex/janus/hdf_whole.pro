pro hdf_whole, filein
if n_elements(filein) lt 1 then filein= '\\lasp-smb\leblanc\CALNEX\goes\NESDIS\geocatL2.GOES-11.2010136.173000.hdf'
fileid=hdf_sd_start(filein,/read)
HDF_SD_FileInfo, FileID, num_datasets, num_attributes

print, 'Attributes:', num_attributes
for i=0, num_attributes-1 do begin
  HDF_SD_AttrInfo, FileID, i, name = attr_name, data = attr_data
  print, attr_name,'  :  ', attr_data
endfor

print, 'datasets:', num_datasets
for i=0, num_datasets-1 do begin
   datasetID = HDF_SD_Select(fileID, i) 
   HDF_SD_GetInfo, datasetID,name = dataset_name, natts = num_attributes,ndim=num_dims, dims =dimvector
   print, i, '   :   ',dataset_name, '  natts:  ',num_attributes, '  dims:  ', dimvector
endfor
nameofdata=''
read, nameofdata, prompt='enter name of the dataset to choose:'
dataset_index = HDF_SD_NameToIndex(fileID, nameofdata)
datasetID = HDF_SD_Select(fileID, dataset_index) 
HDF_SD_GetInfo, datasetID,name = dataset_name, natts = num_attributes,ndim=num_dims, dims =dimvector
for i=0, num_attributes-1 do begin
  HDF_SD_AttrInfo, datasetID, i, name = attr_name, data = attr_data
  print, i,'   :   ',attr_name,'  :  ', attr_data
endfor
HDF_SD_GetData, datasetID, data_variable

help, data_variable
stop
end