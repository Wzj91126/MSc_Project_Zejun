##### P model #####
from matplotlib import pyplot 
import matplotlib.dates as mdates
import numpy as np
from pyrealm import pmodel
import pandas as pd

file_path = 'D:/Imperial College_Master/Project/P model/Whole/whole_LC1.csv'
ds = pd.read_csv(file_path)

temp = np.array(ds['TA_F'][:])
co2 = np.array(ds['CO2'][:] )        # Note - spatially constant but mapped.
elev = np.array(ds['Elevation'][:] ) # Note - temporally constant but repeated
vpd = np.array(ds['VPD_F_MDS'][:])
fapar = np.array(ds['FPAR_Modis'][:])
ppfd = 0.1763* np.array(ds['SW_IN_F'][:]) # PPFD=60×60×24×10−6kECRSW, where kEC=2.04 µmol J−1  #note-splash 
gpp_site = np.array(ds['GPP_DT_VUT_USTAR50'][:]) ## filter
time = np.array(ds['TIMESTAMP'][:])
patm = pmodel.calc_patm(elev)

env = pmodel.PModelEnvironment(tc=temp,co2=co2, patm=patm, vpd=vpd)
model = pmodel.PModel(env)
env.summarize()

modis_gpp = model.estimate_productivity(fapar=fapar, ppfd=ppfd)
modis_gpp =model.gpp

df = pd.DataFrame(time)
df['TIMESTAMP'] = pd.to_datetime(time)
df['modis_gpp'] = modis_gpp
df['site_gpp'] = gpp_site  
df['FPAR'] = np.array(ds['FPAR_Modis'][:])
df['SIF'] = np.array(ds['new_column'][:])
df['site'] = np.array(ds['site'][:])
df.to_csv ('total.csv',index = False,sep=',')



#### GPP hotspot Figure #####
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from scipy.stats import gaussian_kde
from matplotlib.colors import ListedColormap
from scipy.stats import linregress
import seaborn as sns

# input data
fn = r'D:\Imperial College_Master\Project\Final\hotspot\modis.csv'
df = pd.read_csv(fn)
df.dropna(inplace=True)
print(df)

# density
x,y = df.modis_gpp.values,df.site_gpp.values
xy = np.vstack([x,y])
z = gaussian_kde(xy)(xy)
np.save(r'D:\Imperial College_Master\Project\Final\热点图\zarr.npy',z)
# z = np.load(r'D:\ForestMeteorology\FM230731scatter\data\zarr.npy')
idx = z.argsort()
x, y, z = x[idx], y[idx], z[idx]

# linear relationship
slope, intercept, r_value, p_value, std_err = linregress(x, y)
# R^2
r_squared = r_value**2
# REMS
rmse = np.sqrt(np.mean((y - (slope*x + intercept))**2))
# Bias
bias = np.mean(y - (slope*x + intercept))
fig, ax = plt.subplots(figsize=(6,6),dpi=100)

# color bar
# colors = ['#bebebe', '#9f95aa', '#806d97', '#614483'
#           , '#5c3d80', '#57367d', '#523079', '#4d2976'
#           , '#472273', '#421b70', '#3d156c', '#380e69'
#           , '#330766', '#5a0b4e', '#800f35', '#a7131d'
#           , '#ce1704', '#d63c05', '#de6206', '#e58706'
#           , '#edad07', '#f5d208']
colors = ['#bebebe', '#a89eac', '#927f99', '#7c5f87', '#653f75', '#4f2062', '#390050'
          , '#3d005c', '#410067', '#43006d', '#49007e', '#4a0083', '#4f0093', '#510098'
          , '#5500a7', '#5700ab', '#5b00b9', '#5e00c1', '#5f00c5', '#6300d1', '#6601d7'
          , '#6801de', '#6901e1', '#6d01e9', '#6f01ee', '#7202f2', '#7302f4', '#7602f9'
          , '#7802fb', '#7a03fd', '#7b03fd', '#7e03fe', '#8004fe', '#8204fe', '#8405fc'
          , '#8605fa', '#8805f8', '#8906f6', '#8c07f1', '#8d07ed', '#8f08e8', '#9108e2'
          , '#9309dc', '#940ad6', '#960acf', '#970bcb', '#990cbf', '#9b0db7', '#9d0eae'
          , '#9e0ea4', '#a00f9a', '#a21090', '#a31186', '#a41280', '#a61370', '#a81564'
          , '#a91659', '#ab174d', '#ac1841', '#ae1935', '#af1b28', '#b01b22', '#b21e0f'
          , '#b31f03', '#b52100', '#b62200', '#b82400', '#b92500', '#ba2700', '#bc2900'
          , '#bd2b00', '#be2c00', '#c02e00', '#c13000', '#c23200', '#c33300', '#c53700'
          , '#c63900', '#c83b00', '#c93d00', '#ca4000', '#cb4200', '#cd4500', '#ce4700'
          , '#cf4a00', '#d04c00', '#d24f00', '#d35200', '#d45500', '#d55800', '#d65b00'
          , '#d75c00', '#d96100', '#da6400', '#db6700', '#dc6b00', '#dd6e00', '#de7200'
          , '#e07500', '#e17900', '#e27c00', '#e38000', '#e48400', '#e58800', '#e68c00'
          , '#e79000', '#e99400', '#e99600', '#eb9d00', '#eca100', '#eda500', '#eeaa00'
          , '#efaf00', '#f0b300', '#f1b800', '#f2bd00', '#f3c200', '#f4c700', '#f5cc00'
          , '#f6d100', '#f7d700', '#f8dc00', '#f9e200', '#fae400', '#fbed00', '#fcf300'
          , '#fdf900', '#ffff00']

cmap = ListedColormap(colors)

# draw figures
scatter=ax.scatter(x,y,c=z,s = 10,cmap = cmap)
ax.set_xlim(0,25)
ax.set_ylim(0,25)
ax.plot([0,30],[0,30],color= 'k',linestyle=':')
ax.set_xlabel('modis_gpp')
ax.set_ylabel('site_gpp')
sns.regplot(x = 'modis_gpp',y = 'site_gpp',data = df, scatter=False, line_kws={'color': 'red', 'linewidth': .8})

ax.text(1,24,'R$^{2}$ ='+' {:.2f} RMSE = {:.2f} Bais = {:.2f} Slope = {:.2f} N = {}'.format(r_squared,rmse,bias,slope,len(df)))

plt.savefig(r'D:\ForestMeteorology\FM230731scatter\data\scatter.png',dpi = 600)
print('ok')