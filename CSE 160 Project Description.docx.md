***Dataset***\
The dataset is sourced from [***catalog.data.gov***](http://catalog.data.gov), specifically the *Consumer Airfare Report: Table 1a, All U.S. Airport Pair Markets, published by the Bureau of Transportation Statistics*. It contains 657,574 rows and 23 columns. The key attributes we will use include:

-   **Nonstop Distance (miles):** The geographic distance between origin and destination airports, serving as a primary predictor of base fare.\
-   **Passenger Count:** Total number of passengers per route, which reflects demand levels and may correlate with pricing competitiveness.\
-   **Average Fare by Route and Airline:** The primary target variable captures the mean ticket price for a given origin-destination pair operated by a specific carrier.\
-   **Market Share:** The proportion of passengers carried by each airline on a given route, indicating competitive dynamics that influence fare pricing.\
-   **City Market ID:** A standardized identifier for origin and destination cities, used to aggregate and compare traffic across markets and determine the most frequently traveled routes.

It is unsorted for now, so we will need to manually clean the dataset before use. Once cleaned, visualization will involve a scatter plot with prices scattered across the quarter in which the flight takes place. We will also evaluate which factor/attribute is the most likely to affect price, as well as which airport is preferred by the majority of customers.

# ***Visualizations & Analysis***

We will use 2024, as it is the most complete year so far, based on the number of quarters it covers, giving us a more thorough understanding of how flight prices fluctuate over the year. Since 2024 had no major economic events (stock market crashes, etc.) that would significantly affect the average ticket price, it serves as a good baseline for comparison in the coming year. In addition, we will use a heatmap and a box plot that show price distribution by quarter to approximate the lowest price for the specific starting location.

To support both model development and result interpretation, the following visualizations will be produced:

-   **Scatter Plot (Fare vs. Quarter):** Displays the distribution of ticket prices by quarter in 2024, revealing seasonal pricing trends and identifying high- and low-cost travel periods.\
-   **Box Plot (Price Distribution by Quarter):** Illustrates fare variability, median prices, and outliers for each quarter, enabling a clearer comparison of pricing stability across the year.\
-   **Heatmap (Fare by Origin and Quarter):** Maps average ticket prices by departure location and time of year, helping users estimate the cheapest time to fly from a specific region.

In addition to these visualizations, the project will conduct a feature importance analysis to determine which attributes: distance, passenger volume, market share, or quarter, are the strongest predictors of fare price. We will also identify which airports and routes attract the highest passenger volumes, indicating consumer preference and market concentration.

# ***Target Audience & Goals***

Our target audience is mainly passengers looking for low prices, based on their geographical location and the quarter within the United States. Our overall goal is for customers to be able to view the best airlines/airports to book a flight to their desired destination based on predicted ticket prices.
