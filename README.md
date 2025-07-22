## **Lodgic: A Singapore-Based Airbnb Explorer for Visitors and Homeowners**

You can try the website out at the following link: https://synchrownicity.shinyapps.io/Lodgic/

## **Project Overview**

*Lodgic* is an interactive web application built using **RShiny**, designed to support data-driven decision-making in Singapore's vacation rental market. The platform serves two primary user groups:

| User Group | Purpose |
|------------|---------|
| Travellers | Discover listings based on price, room type and proximity to key amenities | 
| Property Owners | Gain insights into local pricing dynamics, demand distribution, and market competition |

Travellers often struggle to find Airbnb listings that align with their specific preferences, while property owners lack access to detailed market intelligence, making it difficult to identify under-served or oversaturated areas.

*Lodgic* addresses these challenges by offering a **centralised, interactive platform** that combines **data visualisation** and **insight-driven analytics** to help both travellers and hosts to make smarter, faster and more informed decisions.

## **How to Test the Project**
You can directly test the Shiny application on the website listed above.

If you would like to make your own changes, take care to follow these steps:
- Clone this repository.
- Open `server.R` and `ui.R` in the same RStudio session.
- You may make your changes in both of these files. `server.R` handles **server logic**, while `ui.R` handles the **front-end logic**.
- Press the `Run App` button at the top right of the Code panel in RStudio. This will run the application.
- See the changes you made!

## **Next Steps**

There are several planned enhancements to further improve the functionality, scalability and depth of analysis provided by *Lodgic*:
- **Real-Time Listings**: Integrate the official Airbnb API to retrieve live listing data, replacing the current static 2024 dataset.
- **Review Sentiment Analysis**: Apply Natural Language Processing (NLP) techniques to analyse user reviews and extract sentiment insights.
- **Enhanced Amenities Data**: Refine and clean the amenities dataset to improve filtering accuracy and user relevance.
- **Improved Scalability**: Containerise the application using Docker to support easier deployment and scaling.
- **Frontend Modernisation**: Explore migrating the UI from R Shiny to React for a more responsive and modular user experience.

## **Project Credits**
Done as part of DBA3702: Descriptive Analytics with R, with the support of Dr. Liu Qizhang.
