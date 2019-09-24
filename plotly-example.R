library(plotly)

d <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/job-automation-probability.csv')

p <- d %>% 
  plot_ly(
    type = 'scatter',
    mode = 'markers',
    x = ~prob,
    y = ~Average.annual.wage,
    marker = list(size = ~numbEmployed, sizeref = 4000, sizemode = 'area'),
    color = ~education,
    text = ~short.occupation,
    hovertemplate = paste(
      "<b>%{text}</b><br><br>",
      "%{yaxis.title.text}: %{y:$,.0f}<br>",
      "%{xaxis.title.text}: %{x:.0%}<br>",
      "Number Employed: %{marker.size:,}",
      "<extra></extra>"
    )
  ) %>%
  layout(legend = list(orientation = 'h', y = -0.3))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link <- api_create(p, filename = "hovertemplate-advanced")
chart_link
