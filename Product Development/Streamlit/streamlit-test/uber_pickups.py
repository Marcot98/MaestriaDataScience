import streamlit as st
import numpy as np
import pandas as pd
import math

st.title('Uber pickups test')

data_source = 'https://s3-us-west-2.amazonaws.com/streamlit-demo-data/uber-raw-data-sep14.csv.gz'
st.cache()
def download_data():
    return  (pd.read_csv(data_source)
      .rename(columns={'Lat':'lat', 'Lon' : 'lon'}))

#df = download_data().loc[0:1000]
df = download_data()
page_size = 1000
total_size = math.ceil(len(df)/1000)
starting_value = 0
slider = st.slider('Select the page', 1, total_size)
#inf,sup = st.slider('Select the Range ',1, 1000, (1,2)
st.write('page selected', slider, 'with limits ', (((slider - 1) * page_size),(slider * page_size)-1))
df = df.loc[((slider - 1) * page_size): (slider * page_size)-1]
#df = df.loc[0:1000]
df
st.map(df)

# LAYING OUT THE TOP SECTION OF THE APP
hour_selected = st.slider("Select hour of pickup", 0, 23)

# FILTERING DATA BY HOUR SELECTED
DATE_TIME = "Date/Time"
df[DATE_TIME] = pd.to_datetime(df[DATE_TIME])
data = df[df[DATE_TIME].dt.hour == hour_selected]

# FILTERING DATA FOR THE HISTOGRAM
filtered = data[
    (data[DATE_TIME].dt.hour >= hour_selected) & (data[DATE_TIME].dt.hour < (hour_selected + 1))
    ]

hist = np.histogram(filtered[DATE_TIME].dt.minute, bins=60, range=(0, 60))[0]

chart_data = pd.DataFrame({"minute": range(60), "pickups": hist})

# LAYING OUT THE HISTOGRAM SECTION
import altair as alt
import pydeck as pdk
st.write("")

st.write("**Breakdown of rides per minute between %i:00 and %i:00**" % (hour_selected, (hour_selected + 1) % 24))

st.altair_chart(alt.Chart(chart_data)
    .mark_area(
        interpolate='step-after',
    ).encode(
        x=alt.X("minute:Q", scale=alt.Scale(nice=False)),
        y=alt.Y("pickups:Q"),
        tooltip=['minute', 'pickups']
    ).configure_mark(
        opacity=0.2,
        color='red'
    ), use_container_width=True)