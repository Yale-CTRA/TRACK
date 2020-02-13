library(shiny)
library(billboarder)

# Define UI for AKI Calculator app ----
ui <- fluidPage(
    
    # Application title
    column(6, align="center", offset=3,
        titlePanel("The Risk of AKI Calculator for Kids"),
        br(),
        div(style="font-size: 28px", "(TRACK)")
    ),
    
    fluidRow(align="center",
        column(4,
            radioButtons("peds", "General pediatrics admission *", choices=c("No", "Yes"), selected="No", inline=TRUE),
            sliderInput("hosp", "Hospital Time (days)", min=0, max=20, 1, step=0.01),
            sliderInput("creat", "Creatinine level (mg/dL)", min=0.1, max=10, 0.5, step=0.1),
            sliderInput("bun", "Blood urea nitrogen (BUN) level (mg/dL)", min=5, max=200, 15, step=1),
            sliderInput("gluc", "Glucose level (mg/dL)", min=10, max=600, 70, step=1)
        ),
        column(4,
            billboarderOutput("display", width = "400px", height = "300px"),
            br(), br(),
            uiOutput("prob")
        ),
        column(4,
            radioButtons("loop", "Loop diuretics use (any form and dose)", choices=c("No", "Yes"), selected="No", inline=TRUE),
                
            radioButtons("bicarb", "Sodium bicarbonate use (any form and dose", choices=c("No", "Yes"), selected="No", inline=TRUE),
            radioButtons("chemo", "Chemotherapy use (any form and dose)", choices=c("No", "Yes"), selected="No", inline=TRUE),
            radioButtons("alpro", "Alprostadil use (any form and dose)", choices=c("No", "Yes"), selected="No", inline=TRUE),
            radioButtons("rbct", "RBC Transfusion", choices=c("No", "Yes"), selected="No", inline=TRUE)
         )
    ),
    br(),
    fluidRow(align="center",
        column(4, offset=2,
            strong("Chemotherapy:"),
            br(),
            div(style="text-align: center",
                "·  Bleomycin  ·  Bortezomib  ·  Carboplatin  ·  Cisplatin ·",
                br(),
                "·  Cyclophosphamide  ·  Cytarabine  ·  Docetaxel ·",
                br(),
                "·  Doxorubicin  ·  Etoposide  ·  Fluorouracil ·",
                br(),
                "·  Gemcitabine  ·  Ifosfamide  ·  Irinotecan ·",
                br(),
                "·  Methotrexate  ·  Paclitaxel  ·  Vinblastine  ·  Vincristine ·"
            )
        ),
        column(4,
            strong("Loop diuretic:"),
            br(),
            "·  Bumetanide  ·  Furosemide  ·  Torsemide ·",
        ),
    ),
    br(),
    fluidRow(align="center",
         column(6, offset=3,
            uiOutput("footnote"),
            br(), br(),
            div(style="text-align: justify", "This predictive tool was built using data from the Yale-New Haven Children Hospital and   intended to be used in predicting patients at risk of acute kidney injury (AKI) in the next 48 hours. This tool is not yet validated for prospective use and should not be used to guide patient care or substituted for clinical judgment. This research was supported by the National Institute of Diabetes and Digestive and Kidney Diseases (grants R01DK113191 and P30DK079310), the career development grant K08DK110536, and the Charles H. Hood Foundation, Inc., Boston, MA."),
            br(), br(),
            uiOutput("website")
         )
    )
)

# Define server logic to calculate probability of AKI ----
server <- function(input, output) {
    
    allInputs <- reactive({
        myvalues <- rep(0, length(names(input)))
        myorder <- c("gluc", "chemo", "peds", "alpro", "creat", "hosp", "loop", "rbct", "bun", "bicarb")
        names(myvalues) <- myorder
        for(name in names(input)){
            iter.match <- FALSE
            for (i in 1:10){
                if (name == myorder[i]){
                    iter.match <- TRUE
                }
            }
            if(iter.match){
                if(input[[name]] == "Yes"){
                    myvalues[[name]] = 1
                } else if (input[[name]] == "No"){
                    myvalues[[name]] = 0
                } else if (!is.na(as.numeric(input[[name]]))){
                    myvalues[[name]] = as.numeric(input[[name]])
                } else {
                    print('Issue detected')
                }
            }
        }
        theta <- c(0.003262, 0.821921, -1.29355, 1.113824, 0.752208, -0.64517, 0.810097, 0.680983, 0.029493, 0.857802)
        b <- -4.05392
        myvalues[6] <- log(myvalues[['hosp']]+1)
        z.hat <- sum(theta*myvalues[myorder]) + b
        p.hat <- 1/(1+exp(-z.hat))
        p.hat
    })
    
    output$prob <- renderUI({
        strong("(The probability of getting AKI within the next 48 hours)")
    })
    
    output$footnote <- renderUI({
        strong("* General pediatrics admission includes patients not admitted to the PICU, NICU, or oncology floor.")
    })
    
    output$website <- renderUI({
        HTML(sprintf("For more information, please visit %s", a("PATR.", href="https://medicine.yale.edu/intmed/patr/")))
    })
    
    output$display <- renderBillboarder({
        billboarder() %>% 
            bb_gaugechart(
                value = round(allInputs()*100,2),
                steps = c(8, 24, 100),
                steps_color = rev(c("#FF0000","#F6C600", "#60B044"))
            ) %>% 
            bb_gauge(
                expand = FALSE,
                min = 0, max = 100,
                label = list(
                    format = suffix("%")
                )
            )
    })
}

shinyApp(ui, server)