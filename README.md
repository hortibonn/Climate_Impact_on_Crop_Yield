# Climate_Impact_on_Crop_Yield
# Auswirkungen des Klimawandels auf Ernteerträge

Entscheidungshilfe für die Landwirtschaft (R Shiny App)Dieses Repository enthält eine R Shiny-Anwendung, die **zukünftige Ernteerträge unter den Bedingungen des Klimawandels für das Jahr 2075** für ausgewählte Kulturen simuliert, derzeit:
- **Spargel**
- **Zwiebeln**
- **Erdbeeren** werden in Kürze hinzugefügt.

Die App verwendet eine **Monte-Carlo-Simulation** (über das Paket „decisionSupport“), um zu untersuchen, wie sich Unsicherheiten bei den Eingabeparametern und klimatischen Bedingungen unter verschiedenen Klimaszenarien auf die Ernteerträge um das Jahr **2075** auswirken.
Die Benutzeroberfläche und der Text sind überwiegend in **Deutsch**, da die App für ein Projekt entwickelt wurde, das sich mit der deutschen Landwirtschaft befasst.

---## Hauptmerkmale

- **Dynamische Eingabeschnittstelle**

Die UI-Elemente werden aus einer Excel-Datei mit `dynamic_helper_v3.R` generiert. Benutzer können:
- Eine Kulturpflanze auswählen
- Variablen nach **Fachgebiet** filtern
- Parameterbereiche interaktiv anpassen

- **Kulturspezifische Modelle**

- **Spargelmodell**: Verwendet spargelspezifische Simulationsfunktionen und Diagramme.
- **Zwiebelmodell**: Verwendet zwiebel-spezifische Klimawirkungs- und Ertragsmodelle, einschließlich einer Heatmap zur Ertragsreduktion.

- **Probabilistische Simulation**

- Führt eine Monte-Carlo-Simulation über `decisionSupport::mcSimulation` durch
- Die Anzahl der Durchläufe ist benutzerdefiniert
- Verwendet aktuelle UI-Parameterbereiche als Eingabeverteilungen

- **Diagramme und Visualisierung**

- Konzeptuelles Modellbild pro Kulturpflanze
- Kulturspezifische Verteilungen, VIP-Diagramme (Variable Importance).

- **Projektstatus speichern/laden**

- Benutzer können:   
    - Speichern Sie die aktuellen Eingabeeinstellungen unter einem benutzerdefinierten Projektnamen.
    - Laden Sie zuvor gespeicherte Zustände.
    - Laden Sie die aktuelle Eingabetabelle als CSV-Datei herunter.

- **Kontinuierliche Verbesserung**

- Benutzer können über das Feedback-Feld Verbesserungsvorschläge einreichen.


## In English
# Agriculture Decision Support Tool (R Shiny App)

This repository contains an R Shiny application that simulates **future crop yields under climate change for the year 2075** for selected crops, currently:

- **Spargel (asparagus)**
- **Zwiebel (onion)**
- **Erdbeere (strawberry)** to be added soon.


The app uses a **Monte Carlo simulation** (via the `decisionSupport` package) to explore how uncertainty in input parameters and climatic conditions affects yield outcomes around the year **2075**, under different climate scenarios. 
The interface and text are primarily in **German** as it is developed for the project involving German agriculture.

---

## Main Features

- **Dynamic input interface**  
  UI elements are generated from an Excel file using `dynamic_helper_v3.R`. Users can:
  - Select a crop  
  - Filter variables by **expertise category**  
  - Adjust parameter ranges interactively

- **Crop-specific models**
  - **Asparagus model**: Uses asparagus-specific simulation functions and plots.
  - **Onion model**: Uses onion-specific climate impact and yield models, including a yield reduction heatmap.

- **Probabilistic simulation**
  - Runs Monte Carlo simulation via `decisionSupport::mcSimulation`
  - Number of runs is user-defined
  - Uses current UI parameter ranges as input distributions

- **Plots & visualisation**
  - Conceptual model image per crop 
  - Crop-specific distributions, VIP (Variable Importance) plots.

- **Save / load project states**
  - Users can:
    - Save their current input settings with a custom project name
    - Load previously saved states
    - Download the current input table as a `.csv`.
    
- **Continuous Improvement**
  - Users can suggest improvements via the feedback box
---

