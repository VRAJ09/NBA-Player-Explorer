<br />
<div align="center">
  <a href="https://github.com/VRAJ09/nba-player-explorer">
    <img src="https://nbcsports.brightspotcdn.com/dims4/default/45f2d3a/2147483647/strip/false/crop/2916x1640+0+0/resize/1486x836!/quality/90/?url=https%3A%2F%2Fnbc-sports-production-nbc-sports.s3.us-east-1.amazonaws.com%2Fbrightspot%2F44%2Fef%2F2dc745d9e60683818039ca029fdf%2Fap-101206125847-e1527112035702.jpg" alt="logo">
  </a>

<h3 align="center">NBA Player Explorer: Shot Charts & Advanced Analytics (2024–25)</h3>

  <p align="center">
     <br />
    <a href="https://github.com/VRAJ09">Vraj Patel</a>
  <p>
  <p align="center">
                  Interactive NBA Performance Visualization Tool
  </p>
   <p align="center">
      Analyze shot charts, compare advanced stats, predict playoff performance, and explore player similarity using PCA — all in one Shiny app.
  </p>
</div>

<div align="center">

[![RStudio](https://img.shields.io/badge/Built%20With-RStudio-75AADB?logo=rstudio)](https://posit.co)
[![Shiny](https://img.shields.io/badge/Interactive%20App-Shiny-1E9BB5?logo=R)](https://shiny.posit.co/)
<br>

</div>

---

## 🧠 Abstract
<p>
This project delivers a fully interactive NBA performance analysis dashboard using R and Shiny. It combines shot location data from the 2024–2025 NBA season with statistical metrics from the 2023–2024 regular season and playoffs. Users can explore player shot maps, analyze stat trends, view top performers, predict playoff stats from regular season data, and visualize player similarities using PCA.
</p>

<p>
The Shot Chart tool renders court maps with filters by player, shot type, zone range, and date. Statistical dashboards include sortable tables, percentile radar charts, and visual comparisons. A Playoff Prediction feature uses linear regression to show predicted playoff stats alongside actual ones. PCA mapping offers a 2D projection of players based on multivariate analysis. The goal is to create a sleek, informative environment for fans, analysts, and developers to understand player tendencies and performance trends.
</p>

---

## 📁 Dataset Information

**📊 Shot Data:** NBA.com API (2004–2025)  
**📈 Player Stats:** Basketball Reference (2023–2024 Regular & Playoffs)

Files:
- `Data/NBA_2025_Shots.csv`
- `Data/2023-2024_NBA_Player_Stats_Regular.csv`
- `Data/2023-2024_NBA_Player_Stats_Playoffs.csv`

---

## 📂 File Structure

```
nba-player-explorer/
├── NBA_Player_Explorer.R                         # Complete Shiny app script
├── Data/
│   ├── NBA_2025_Shots.csv
│   ├── 2023-2024_NBA_Player_Stats_Regular.csv
│   └── 2023-2024_NBA_Player_Stats_Playoffs.csv
└── README.md
```

---

## 💻 Setup Instructions

### 1. Clone the Repository
```bash
git clone https://github.com/VRAJ09/NBA-Player-Explorer.git
cd NBA-Player-Explorer
```

### 2. Install Required R Packages
```r
install.packages(c("shiny", "ggplot2", "plotly", "DT", "dplyr", "tidyr", "fmsb", "png", "grid", "ggimage"))
```

### 3. Run the App
```r
shiny::runApp(".")
```

Make sure all CSVs are inside the `Data/` folder as expected.

---

## 📊 Key Features

- 🏀 **Shot Charts**: Interactive half-court visualizations by player & game
- 📈 **Stat Comparison**: Compare players across selected metrics
- 📋 **Player Tables**: Sortable tables with custom stats
- 🌟 **Top 10 Stats**: Visual bar charts of the top performers
- 🧭 **Radar Charts**: Percentile-based visualization of player strengths
- 🔮 **Playoff Prediction**: Linear regression for stat forecasting
- 🔬 **PCA Mapping**: Visual player clustering based on performance
