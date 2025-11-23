# ds456-capstone_aki-hanna-yosephine

Aki Wada, Hanna Chang, and Yosephine Manihuruk

## Research Question

> How can we identify redundant/unique segments in public transit routes and micro mobility data in Minneapolis, and how can we use the observations to optimize inter-Minneapolis travel?

## Motivation

Our project stemmed from our shared interest in spacial data analysis, one of its specific trait being its versatility. We agreed upon transportation, and more specifically public transit and micro mobility.

With the rise of urbanization, cities are facing increasing challenges in managing transportation systems efficiently. Redundant routes can lead to wasted resources, increased traffic congestion, and higher emissions. By identifying and optimizing these routes, we can improve the overall efficiency of public transit and micro mobility options in Minneapolis, making it easier for residents to navigate the city while reducing environmental impact.

## Files

| File | Description |
|-------------------------------------|-----------------------------------|
| `DataConcat.qmd` | Code for connecting public transit data, with cleaning. |
| `TransitExp.qmd` | Code for GTFS data familiarity and exploratory visualizations |
| Exploratory/`Yosephine_FP2.zip` | Contains `.qmd` and `.html` files along with datasets. This FP2 shows two viz: *commuting patterns “by other means” in the Twin Cities metro area* using census data and *daily bike rider counts in 2022 for Hennepin, Ramsey, and Washington Counties*. | 
| Dashboard/`01_data_prep.md` | Code to clean and prep data files. |
| Dashboard/`02_trip_density.md` | Code to visualize trip start times density plot. |
| Dashboard/`03_trip_proximity.md` | Code to wrangle more data for following steps. |
| Dashboard/`04_overlaps.md` | Code to identity route overlaps, and export the data. |
| Dashboard/`dashboard_prototype.md` | Code for the data dashboard prototype (draft). |
| Dashboard/data | All data exports created from the `.md` codes in Dashboard folder. |

## Datasets/Data Source

| File/Folder | Description | Source Link |
|-------------------------|-----------------------|-------------------------|
| Data/`GTFS20201128.zip` | **Must be downloaded and upzipped. File size too large to upload to repo.** <br><br> Contains detailed information about public transit routes, stops, and schedules in Minneapolis in the week of November 28, 2020. <br><br>**Notes:** All datasets must be concatenated into one GTFS dataset for analysis for a given timeframe. | <https://gtfs.org/> <br> <https://svc.metrotransit.org/mtgtfs/archive/> |
| Data/`MNPSC_City_Boundary` | Shapefile and `.csv` of Minneapolis city boundary. | <https://opendata.minneapolismn.gov/search?groupIds=1390110900e456482f79> |
| Data/`scooter_trips_2025.csv` | A `.csv` file of micromobility in the Twin Cities area. | <https://opendata.minneapolismn.gov> via data request |

## Rest Of The Semester Plan

**Nov 10:**

-   [x] Finalize data exploration and exploratory data visualization (All)

-   [x] Group meeting for the intermediate class presentation

-   [x] Class presentation slides prep. (Aki)

-   [x] Intermediate narrative Github repo (Hanna)

**Nov 11:**

-   [ ] Group meeting prior to class, wrap up and rehearse for intermediate class presentation

-   [x] Begin construction for final visualization "deliverable" if time allows

-   [x] Intermediate class presentation (All)

**Nov 11-15:**

-   [x] Final deliverable construction (individual + coordinate via groupchat) (All/Yosephine)

    -   [x] Yosephine to share starter code

-   [x] Begin brainstorming and drafting analysis methods (Hanna)

- [ ] Create outline for final report (Aki)

**Nov 16:**

-   [x] Group meeting to discuss progress on final deliverable and analysis methods

**Nov 17:**

-   [ ] Meeting with Max Gonzalez & Jacob Wascalus (expert meeting as required per class schedule, due 8am Nov 18) (All)
        - Cancelled

-   [ ] Post-meeting notes and action items into a document for submission (Hanna)

**Nov 23:**

-   [x] Group meeting

**Nov 24**
-   [ ] Group meeting with instructor

**Nov 26-30**
*Break*
-   [ ] Micomobility & public transit route gaps identification (Hanna)
-   [ ] Debug and troubleshoot data dashboard (Yosephine)
-   [ ] Continue final narrative (Aki)

**Dec 04**
-   [ ] Meeting with Jacob Wascalus @ 11am

## Group Member Contributions to the Intermediate Materials Assignment

| Name | Contribution Description |
|------------------|------------------------------------------------------|
| **Aki Wada** | \- Prepared slides for the Intermediate Presentation, including all necessary information relayed by group members.<br> - Contacted Max Gonzalez for a second follow-up on the progress of the data request. |
| **Hanna Chang** | \- Completed the Intermediate Narrative, including tentative scheduling for the rest of the semester.<br> - Worked on the GitHub repository for narrative submission and file/code reproducibility. |
| **Yosephine Manihuruk** | \- Initiated in-person group meeting prior to presentation day.<br> - Fine-tuned the direction of the project upon data exploration and investigation. |
