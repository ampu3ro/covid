# COVID-19

There is no shortage of great data visualizations to help us understand the ongoing pandemic, but there are also always more angles to explore. And while I have only a basic understanding of this topic, based mostly on articles from major US new outlets, I think experts and the interested public can certainly benefit from new takes, even if rough or incomplete. To that end, I thought I'd try to contribute in my own small way.

## Hospital bed utilization

As an avid consumer of data viz, I found the case/death trackers (e.g. the [dashboard](https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html) published by the New York Times) informative and well communicated, but lacking the crucial bit of context of when a given geographic area is reaching its limits. My first take on a new perspective to tracking the progression of the virus attempts to frame the data in terms of what public health experts seem to be most concerned about: a shortage a hospital resources at any given point in time.

This is of course a multi-facetted concern, ranging from hospital staffing and budgeting, to personal protective equiment, to physical space and equipment avaialable. ProPublica published an interesting [article](https://projects.propublica.org/graphics/covid-hospitals) on the latter topic using hospital capacity [data](https://globalepidemics.org/our-data/hospital-capacity/) compiled by the Harvard Global Health Institute. It's a scenario analysis of where the the virus could put outsized strain on a given "Hospital Referral Region" (each of which provides health care for ~65% of the population within its boundries). The possibile outcomes are wide ranging, though, and it's not clear which case is the most likely. But, combined with up-to-date tracking [data](https://github.com/nytimes/covid-19-data) compiled by the NYT, the trajectory of the situation comes more into focus and "true" hotspots (where resources are really strained) are easier to identify.

![](plots/Northeast.gif)

![](plots/West.gif)

Here _hospital bed utilization_ is defined as _(estimated hospital beds occupied prior to COVID-19 + beds added daily due to COVID-19 - beds no longer needed after average length of stay) / total beds in an HRR region_ where beds needed due to COVID-19 assumes 20% hospitalization of cases and a 12 day stay (both taken from HGHI). To be clear, these are very much estimates. Even the number of beds available prior to the pandemic is rough and optimistic in the sense that it assumes the half the beds used could be freed up for the surge. Additionally, counties don't always fit neatly within HRR regions so cases are apportioned based on area overlap. Nevertheless, I find the resulting graph to be useful in terms identifying patterns and trends with context.

## Neighborhood sidewalk areas

Even as lockdowns are gradually lifted throughout the country, social/physical distancing will continue to be an invaluable tool for keeping the strain on hospitals at bay. CDC recommends a distance of six feet in order to limit the spread of the virus, but as people venture outside more, both to go about their resumed daily lives and to take advantage of the warmer weather, cities are starting to realize that there may not be sufficient walking space for residents to adhere to the public health guidelines.

Some urbanists have noted that this pandemic may provide a unique [opportunity](https://www.wired.com/story/pandemic-opportunity-remake-cities/) to redesign cities by [reclaiming](https://www.theatlantic.com/ideas/archive/2020/04/pandemic-shows-what-cities-have-surrendered-cars/610423/) street space from cars and give precedence back to pedestrians (and cyclists), encouraging social integration, economic activity, and healthier lifestyles. Realististically though, at least in the near-term, most streets will stay car centric so sidewalks will have to suffice as public space for residents in transit or exercising.

Intuitively, there can obviously be large differences within cities (and between cities) for available sidewalk space, but quantified, they reveal some interesting insights.

![](plots/Neighborhood.png)

I chose to focus on Boston nieghborhoods, both because the sidewalk [data](https://data.boston.gov/dataset/sidewalk-inventory) was readily available through the city's useful and well-designed data portal, and because it's been home for the past 12 years so I can gut check the data better.

The sidewalk space is normalized by population as neighborhood residents will presumably be the large majority of total pedestrians for the near-term. Although it seemed as though there was some correlation between sidewalk area/width and income (West Roxbury, Hyde Park and Roslindale have mostly low width sidewalks), the apparent pattern wasn't really substantiated by the data.


_This is an ongoing project that I'm sure will improve/expand over time, but I hope it at least offers some interesting approaches to an important topic. Please let me know if there are any concerns with the accuracy of the data or how it's being presented. Thanks!_
