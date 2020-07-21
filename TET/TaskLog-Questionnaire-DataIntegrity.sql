 -- --------------------------            
-- taskLog: Eligibility must have 1 or 2  distinct tasks for TET
-- --------------------------     
#183 participants have 2 task in Eligibility
-- select count(distinct(task_name)) as task_count, study_id from  task_log where  
--   		study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 		 and session_name = 'Eligibility' 
-- 		 group by study_id   
--          having task_count = 2
-- --------------------------     
#66 participants have one task in Eligibility
-- select count(distinct(task_name)) as task_count, study_id from  task_log where  
--   		study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 		 and session_name = 'Eligibility' 
-- 		 group by study_id   
--          having task_count = 1
-- --------------------------                        
-- select * from task_log where 
-- 	study_id = 83330 
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0))       
 -- --------------------------     
 -- --------------------------     
-- taskLog: preTest must have 14 distinct tasks
-- --------------------------        
#180 participant finished preTest    
-- select count(distinct(task_name)) as task_count, study_id from  task_log where  
-- 	study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
--     and session_name = 'preTest' 
--     group by study_id
--     HAVING  task_count = 14
-- --------------------------            
#18 participants have OA as well as other tasks
-- select count(distinct(task_name)) as task_count, study_id from  task_log where  
-- 			study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
--             and session_name = 'preTest' 
--             group by study_id  
--             HAVING  task_count > 14
-- --------------------------            
#1 participants are in the middle of preTest
-- select count(distinct(task_name)) as task_count, study_id from  task_log where  
-- 			study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
--             and session_name = 'preTest' 
--             group by study_id  
--             HAVING  task_count = 11 

-- --------------------------                        
 -- select * from task_log where study_id = 83474 and  session_name = 'preTest'  and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- --------------------------         
-- --------------------------           
-- taskLog: firstSession must have 8 distinct tasks for TET
-- --------------------------            
-- select count(distinct(task_name)), study_id from  task_log where  study_id in (select id from calm.study where study_extension = 'TET') and session_name = 'firstSession' group by study_id    
-- --------------------------            
#117 participants finished firstSession
-- select count(distinct(task_name)) as task_count, study_id from  task_log where  
-- 			study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
--             and session_name = 'firstSession' 
--             group by study_id  
--             HAVING  task_count = 8
-- --------------------------            
#40 participants are in the middle of session one
-- select count(distinct(task_name)) as task_count, study_id from  task_log where  
-- 			study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
--             and session_name = 'firstSession' 
--             group by study_id  
--             HAVING  task_count < 8
-- --------------------------                        
-- select * from task_log where study_id = 83351 and  session_name = 'firstSession'  and study_id in (select id from calm.study where study_extension = 'TET')             
-- --------------------------           
-- taskLog: secondSession must have 5 distinct tasks for TET
-- --------------------------            
#63 participants finished Session 2
-- select count(distinct(task_name)) as task_count, study_id from  task_log where 
-- 	study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
--     and session_name = 'secondSession' 
--     group by study_id    
-- 	HAVING  task_count = 5
-- --------------------------                        
#0 participants are in the middell of Session 2
-- select count(distinct(task_name)) as task_count, study_id from  task_log where  
-- 		study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
--         and session_name = 'secondSession' 
--         group by study_id    
--         HAVING  task_count < 5
-- --------------------------                        
 -- select * from task_log where study_id = 83330 and  session_name = 'secondSession'  and study_id in (select id from calm.study where study_extension = 'TET')       
 -- --------------------------   
-- --------------------------            
-- taskLog: thirdSession must have 16 distinct tasks for TET
-- --------------------------     
#31 participants finished Session 3             
--  select count(distinct(task_name)) as task_count, study_id from  task_log where  
--  		study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
--         and session_name = 'thirdSession'
--         group by study_id    
--         HAVING  task_count = 16
-- --------------------------     
#5 participants are in the middle Session 3               
--  select count(distinct(task_name)) as task_count, study_id from  task_log where  
--   		study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
--         and session_name = 'thirdSession' 
--         group by study_id    
--         HAVING  task_count < 16
-- --------------------------                        
  -- select * from task_log where study_id = 83330 and  session_name = 'thirdSession'  and study_id in (select id from calm.study where study_extension = 'TET')       
 -- --------------------------           
 -- --------------------------            
-- taskLog: fourthSession must have 5 distinct tasks for TET
-- --------------------------     
#18 participants finished Session 4            
--  select count(distinct(task_name)) as task_count, study_id from  task_log where  
--  		study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
--         and session_name = 'fourthSession' 
--         group by study_id    
--         HAVING  task_count = 5
-- --------------------------     
#0 participants are in the middle Session 4         
 -- select count(distinct(task_name)) as task_count, study_id from  task_log where  
--  		study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
--         and session_name = 'fourthSession' 
--         group by study_id    
--         HAVING  task_count < 5
-- --------------------------                        
-- select * from task_log where study_id = 83355 and  session_name = 'fourthSession'  and study_id in (select id from calm.study where study_extension = 'TET')       
 -- --------------------------      
 -- --------------------------            
-- taskLog: fifthSession must have 18 distinct tasks for TET
-- --------------------------     
#11 participants finished Session 5           
 -- select count(distinct(task_name)) as task_count, study_id from  task_log where  
--   		study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 		 and session_name = 'fifthSession' 
-- 		 group by study_id   
-- 		 HAVING  task_count = 18
-- --------------------------     
#0 participants are in the middle Session 5 (no Covid19)       
--  select count(distinct(task_name)) as task_count, study_id from  task_log where  
--  		study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
--         and session_name = 'fifthSession' 
--         group by study_id   
--         HAVING  task_count < 18
-- --------------------------                        
-- select * from task_log where study_id = 83320 and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0))       
 -- --------------------------       
 -- --------------------------            
-- taskLog: PostFollowUp must have 12  distinct tasks for TET??? (no data exist for this)
-- --------------------------     

