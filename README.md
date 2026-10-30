# DS2_XC_Shiny
R shiny app that simulates NCAA XC races based off of past results

NOTE: This project relies on calling an API running locally. Follow instructions below to set up the local API call.
CREDIT to 'https://github.com/matthewmorena/tfrrs-scraper' for the tfrrs API.

This shiny app was created with the goal of visualizing predicted collegiate cross country meet scores, allowing
users to see simulate races between schools of their choice.

The interactive checkbox allows users to select which teams they want to be in a hypothetical race to simulate.
The default tab shows users the individual results of a hypothetical meet between the selected teams based off
of the runners most recent 8k performance. The "Team Results" tab allows users to easily visualize the team score
as well as a breakdown of how each teams runners would score in this hypothetical meet.


*Instructions to set up API*:
  -1. Open terminal and navigate to the 'tffrs-scraper' folder.
  -2. Create and activate a python virtual environment
           venv\Scripts\activate
  -3. Install the required packages
        pip install -r requirements.txt
  -4. Run the API
        uvicorn main:app --reload
        
The API should now be successfully running on 'http://127.0.0.1:8000'.