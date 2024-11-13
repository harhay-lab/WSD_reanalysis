fsfun=function(sda,y.name,tr.name,xnames){

soi=sace(sda,y.name,tr.name,xnames)
lsi=length(soi)

return(soi[(lsi-3):lsi])

return(est)
}

csfun=function(sda,y.name,tr.name,xnames,cluster.name){

coi=csace(sda,y.name,tr.name,xnames,cluster.name)
lci=length(coi)
cpi=coi[(lci-2):lci]

sat=coi[lci-5]
taut=coi[lci-4]
oicc=taut/(taut+sat)
sace=coi[lci-3]

return( c(sace,oicc,cpi))
}

rcsfun=function(sda,y.name,tr.name,xnames,cluster.name){

roi=rsace(sda,y.name,tr.name,xnames,cluster.name)
lri=length(roi)
sace=roi[lri-3]
micc=roi[lri-4]/(roi[lri-4]+pi^2/3)
oicc=roi[lri-5]/(roi[lri-5]+roi[lri-6])

return(c(sace,oicc,micc,roi[(lri-2):lri]))
}


