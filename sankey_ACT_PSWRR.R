require(rCharts)
require(igraph)
library(dplyr)
library(tidyr)
library(ggthemes)
library(reshape2)

nodes = c("High School Graduates",
          "ACT 0-15",
          "ACT 16-18",
          "ACT 19-20",
          "ACT 21-26",
          "ACT 27-36",
          "Did not enroll",
          "Technical College",
          "Community College",
          "Four-year University"
          )

g<-graph
g<-graph(c(1,2, 1,3, 1,4, 1,5, 1,6,
           2,7, 2,8, 2,9, 2,10,
           3,7, 3,8, 3,9, 3,10,
           4,7, 4,8, 4,9, 4,10,
           5,7, 5,8, 5,9, 5,10,
           6,7, 6,8, 6,9, 6,10
           ))
          

sankey <- read_csv("K:/Research and Policy/projects/hs_feedback_report/Derived Data File/Mock District Data 20170412.csv") %>%
  filter( page =="7c") %>%
  select(act_recode, institution_level, hsgrad_state, hsgrad, enroll_state, enroll_bin) %>% 
  group_by(act_recode) %>% 
  mutate(HS_Grad_Score = sum(hsgrad)) %>% 
  ungroup() %>% 
  group_by(act_recode, institution_level) %>% 
  mutate(HS_Grad_Enroll = sum(hsgrad)) %>% 
  ungroup() %>% 
  mutate(institution_type = ordered(institution_level, 
                                    levels = c("Four-year University", "Community College", "Technical College", "Did not enroll")),
         ACT_score = ordered(act_recode, 
                                    levels = c("0-15", "16-18", "19-20", "21-26","27-36"))) %>%
      select(ACT_score, institution_type, HS_Grad_Score, HS_Grad_Enroll) %>%  
  distinct(ACT_score, institution_type, HS_Grad_Score, HS_Grad_Enroll) %>%
  arrange(ACT_score, desc(institution_type)) 

attach(sankey)
E(g)$weights<- c(HS_Grad_Score[1], 
                 HS_Grad_Score[5],
                 HS_Grad_Score[9],
                 HS_Grad_Score[13],
                 HS_Grad_Score[17],
                 HS_Grad_Enroll[1],
                 HS_Grad_Enroll[2],
                 HS_Grad_Enroll[3],
                 HS_Grad_Enroll[4],
                 HS_Grad_Enroll[5],
                 HS_Grad_Enroll[6],
                 HS_Grad_Enroll[7],
                 HS_Grad_Enroll[8],
                 HS_Grad_Enroll[9],
                 HS_Grad_Enroll[10],
                 HS_Grad_Enroll[11],
                 HS_Grad_Enroll[12],
                 HS_Grad_Enroll[13],
                 HS_Grad_Enroll[14],
                 HS_Grad_Enroll[15],
                 HS_Grad_Enroll[16],
                 HS_Grad_Enroll[17],
                 HS_Grad_Enroll[18],
                 HS_Grad_Enroll[19],
                 HS_Grad_Enroll[20])
          
detach(sankey)
# convert to data frame with appropriate node names
edgelist <- get.data.frame(g)

# name columns as what is expected by plugin
colnames(edgelist) <- c("source", "target", "value")
edgelist$source <- lapply(edgelist$source, FUN = function(x) {nodes[x]})
edgelist$target <- lapply(edgelist$target, FUN = function(x) {nodes[x]})

## now we plot
sankeyPlot <- rCharts$new()
sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey/libraries/widgets/d3_sankey')
sankeyPlot$set(
  data = edgelist,
  nodeWidth = 15,
  nodePadding = 25,
  layout = 32,
  width = 400,
  height = 300
)

sankeyPlot$setTemplate(
  afterScript = "
  <script>
  d3.selectAll('#{{ chartId }} svg path.link')
  .style('stroke', function(d){
  if
  (d.source.name=='Below Basic or Basic Gr 3' & d.target.name=='Proficient or Advanced Gr 5')
  {return('#008000');} 
if
  (d.source.name=='Proficient or Advanced Gr 3' & d.target.name=='Below Basic or Basic Gr 5')
  {return('#FF0000');}
if
  (d.source.name=='Below Basic or Basic Gr 3' & d.target.name=='Below Basic or Basic Gr 5')
  {return('#808080');}
if
  (d.source.name=='Proficient or Advanced Gr 3' & d.target.name=='Proficient or Advanced Gr 5')
  {return('#808080');}
})
  </script>
  ") 


sankeyPlot


sankeyPlot$setTemplate(
  afterScript = "
  <script>
  
  var cscale = d3.scale.ordinal()
  .domain(['N','E', 'G' ,'I','T','N ','E ', 'G ' ,'I ','T '])
  .range(['#d3d3d3', '#32ca32', '#1f78b4', '#e31a1c','#ecd736','#d3d3d3',    
                     '#32ca32', '#1f78b4', '#e31a1c','#ecd736'])
  
  
  
  d3.selectAll('#{{ chartId }} svg path.link')
    .style('stroke', function(d){
    return cscale(d.source.name);
    //returns grey links
  })
  
  d3.selectAll('#{{ chartId }} svg .node rect')
     .style('fill', function(d){
     return cscale(d.name)
  })
  .style('stroke', 'none')
  
  var text_box = d3.selectAll('#{{ chartId }}').append('svg')
  .attr('width', 500)
  .attr('height', 100)
  
  var TextData = [
  { 'cx': 0, 'cy': 20 ,'label': 'High School Grads', 'pos': 'left'},
  { 'cx': 250, 'cy': 20,'label': 'ACT SCore','pos': 'middle'},
  { 'cx': 500, 'cy': 20, 'label': 'PS Enrollment','pos': 'end'}];
  
  var text = text_box.selectAll('text')
  .data(TextData)
  .enter()
  .append('text');
  
  //Add the text attributes
  var textLabels = text
  .attr('x', function(d) { return d.cx; })
  .attr('y', function(d) { return d.cy; })
  .text( function (d) { return d.label ; })
  .attr('text-anchor', function(d) { return d.pos ;})
  .attr('font-family', 'sans-serif')
  .attr('font-size', '14px')
  .attr('fill', 'black');
  
  </script>
  ")
