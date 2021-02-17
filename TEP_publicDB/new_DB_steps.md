**STEPS ADDING NEW DATABASE:**

1.  Modify `Atlas_dictionary_final.csv` file, save and upload it
    1.  Add a column with the new DB and the different tissue names
        matching the Master column
    2.  Upload modified file to
        “/dlab/NGS/bioinformatics/PCS/DIPGENOMICS/TEP\_Shiny\_data/version\_0.9/data/”
2.  Execute script `All_DB.Rmd`to upload new Atlas dictionary’s version
3.  Include new DB in `All_DB.Rmd` (local path or weblink)
4.  Save the environment including new DB and Atlas Dictionary version
    as `TEP_Shiny.RData`

`ui.R file`

1.  If new specie, add to selecInput(“species”)
2.  Add database to selectInput(“database”)
3.  Add conditionalPanel for the plot (If microarray data, include
    selecInput) within its species
4.  Add database to tabPanel (“Data”) and its species - fluidRow
5.  Add explanation of DB in tabpanel (“Methods”)

`server.R file`

1.  Manipulate dataset to obtain 2 columns (UoM, tissue) for selected
    gene
2.  Create E\_genexxxx
3.  Add to DB\_Summary
    1.  Create 2 rows on if else
    2.  Add DB\_sum to sum\_table
4.  Add db to summary\_enriched function
5.  Add specie to speciesTissue if missing
6.  Add db to heatmap\_sum
7.  Create output$graphDB (eventReactive + renderPlot)
8.  Create renderDataTable output$geneExp\_DB

`global.R file`

1.  Add DB to DBs and Specie vectors for filtering and factor to display
    in desired order
2.  Create entry in DB\_Used

`Report2.Rmd`

1.  Add DB description
2.  Add Figure plot
3.  Add row in DB\_legend depending on number of tissue systems
4.  Add heatmap rows (NO)
5.  Add db summary rows (NO)
