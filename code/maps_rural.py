import plotly.graph_objects as go
import pandas as pd
from urllib.request import urlopen
import json
import plotly

with urlopen("https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/brazil-states.geojson") as response:
 Brazil = json.load(response) # Javascrip object notation 

fig = go.Figure()
layout = dict(
    title_text = "",
)

d_2G_rural = pd.read_csv("dados_2G_Rural.csv",sep=",")
d_3G_rural = pd.read_csv("dados_3G_Rural.csv",sep=",")
d_4G_rural = pd.read_csv("dados_4G_Rural.csv",sep=",")
d_2G_urbano = pd.read_csv("dados_2G_Urbano.csv",sep=",")
d_3G_urbano = pd.read_csv("dados_3G_Urbano.csv",sep=",")
d_4G_urbano = pd.read_csv("dados_4G_Urbano.csv",sep=",")

vetor = [d_2G_rural,d_3G_rural,d_4G_rural]
vetorNomes = ["2G Rural","3G Rural","4G Rural"]
for index, dados in enumerate(vetor):
    geo_key = 'geo'+str(index+1) if index != 0 else 'geo'  
    fig.add_trace(go.Choropleth(
            locations=dados.UF,
            z = dados.populacao,
            geo=geo_key,
            colorscale="Viridis",
            geojson=Brazil,
            featureidkey="properties.sigla",
            zmin = 10,
            zmax = 91,
            colorbar_title = "%",
            name=vetorNomes[index],
        )
    )
    layout[geo_key] = dict(
        domain = dict( x = [], y = [] ),
    )

layout
z = 0
COLS = 3
ROWS = 1
for y in reversed(range(ROWS)):
    for x in range(COLS):
        geo_key = 'geo'+str(z+1) if z != 0 else 'geo'
        layout[geo_key]['domain']['x'] = [float(x)/float(COLS), float(x+1)/float(COLS)]
        layout[geo_key]['domain']['y'] = [float(y)/float(ROWS), float(y+1)/float(ROWS)]
        z=z+1
        if z > 5:
            break
            
fig.update_layout(layout)  
fig.update_geos(fitbounds = "locations", visible = False)
 
plotly.io.write_html(fig, file="rural.html", full_html=False, default_height="80%")