## project: does error type predict quitting?

**Annie Johansson, Maarten van der Velde, Abe Hofman**\
**2026**

<a href="https://docs.google.com/document/d/1m1JwisaO9PHk7jxXOZMjo4v4Ndb9Dxwdpc_kEoT5AX0/edit?usp=sharing" target="_blank">meeting notes & updates</a>

this will be a replication and extension of the quitting effect (seq. errors -> quitting), 
interacted with error type, and explored across math garden and memory lab datasets. 

### analysis ideas

model the probability of a state transition, given: 

 -  previous state 
 -  error (cognitive, non-cognitive)  
 -  error sequence (1, 2, 3, >3)  
 -  person-covariates (age, ability)  
 -  context-specific covariates (home/school, domain)  
 -  surprise (delta)  

where states are defined by engagement (e.g. on-task / off-task; persisting / soft-quitting).

### data

I start by exploring (a training subset of) multiplication data from 2021 to 2024.  

### currently exploring 

-  can errors (cognitive vs. non-cognitive) be modeled as a state?  
-  rt distributions over set predictions 
-  indicators: how to classify cognitive / non-cognitive error types?
-  item characteristics and error systematicity

### configuration

all run settings are centralized in `config.R` at the project root.

this includes:

- hard-coded input/output paths
- data split (`train` / `test`)
- filtering thresholds (e.g., `min_item_count`, `min_play_count`)

edit `config.R` locally to match your own data location.

 
 
 
 
