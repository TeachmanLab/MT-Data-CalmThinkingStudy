-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,  count(distinct session_name) from task_log where 
-- 	task_name = 'credibility'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

#check for duplicaiton in taskLog/table
-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'credibility'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1


-- select count(distinct participant_id) as freq, count(distinct session) from credibility 
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

#check for duplicaiton in taskLog/table
-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		credibility
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) = 1

-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,  count(distinct session_name) from task_log where 
-- 	task_name = 'demographics'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

#check for duplicaiton in taskLog/table    
-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'demographics'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1

-- select count(distinct participant_id) as freq, count(distinct session) from demographics 
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

#check for duplicaiton in taskLog/table
-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		demographics
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1
-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq, count(distinct session_name) from task_log where 
-- 	task_name = 'MentalHealthHistory'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

#check for duplicaiton in taskLog/table
-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'MentalHealthHistory'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1

-- select count(distinct participant_id) as freq, count(distinct session) from mental_health_history 
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

#check for duplicaiton in taskLog/table
-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		mental_health_history
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1
-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,  count(distinct session_name)  from task_log where 
-- 	task_name = 'AnxietyIdentity'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
  
 #check for duplicaiton in taskLog/table 
-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'AnxietyIdentity'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    
    
-- select count(distinct participant_id) as freq, count(distinct session) from anxiety_identity 
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

#check for duplicaiton in taskLog/table
-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		anxiety_identity
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1
-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'OA'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

# 5 participants have duplication in eligilibity session with study ID (83426, 83470, 83472, 83502, 83547)
-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'OA'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    

-- select count(distinct participant_id) as freq, count(distinct session) from OA 
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

# 5 participants have duplication in eligilibity session with participant ID (2112, 2156, 2158, 2188, 2233)
-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		OA
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1

-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'AnxietyTriggers'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 


-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'AnxietyTriggers'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    
   
-- select count(distinct participant_id) as freq, count(distinct session) from anxiety_triggers
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		anxiety_triggers
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1

-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'rr'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'rr'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    
  
-- select count(distinct participant_id) as freq, count(distinct session) from rr
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		rr
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1
-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'bbsiq'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'bbsiq'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    

-- select count(distinct participant_id) as freq, count(distinct session) from bbsiq
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		bbsiq
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1


-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'Comorbid'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'Comorbid'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    

-- select count(distinct participant_id) as freq, count(distinct session) from Comorbid
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		Comorbid
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1
-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'Wellness'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'Wellness'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    

-- select count(distinct participant_id) as freq, count(distinct session) from wellness
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		wellness
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1

-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'Mechanisms'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'Mechanisms'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    

   
-- select count(distinct participant_id) as freq, count(distinct session) from mechanisms
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)


-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		mechanisms
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1


-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'Covid19'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'Covid19'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    
    
-- select count(distinct participant_id) as freq, count(distinct session) from covid19
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)


-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		covid19
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1

-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'TechnologyUse'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'TechnologyUse'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    

-- select count(distinct participant_id) as freq, count(distinct session) from technology_use
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		technology_use
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1


-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'Affect'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'Affect'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
--             -- and session_name = 'firstSession' 
--             and session_name = 'thirdSession'
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    
   
-- select count(distinct participant_id) as freq, count(distinct session) from affect
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		affect
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
--            --  and session = 'firstSession'
--             and session = 'thirdSession'
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1

-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'SessionReview'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'SessionReview'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    

  
-- select count(distinct participant_id) as freq, count(distinct session) from session_review
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		session_review
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1

-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'CoachPrompt'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'CoachPrompt'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    
  
-- select count(distinct participant_id) as freq, count(distinct session) from coach_prompt
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		coach_prompt
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1

-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'ReturnIntention'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'ReturnIntention'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    
   
-- select count(distinct participant_id) as freq, count(distinct session) from return_intention
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		return_intention
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1
-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'HelpSeeking'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'HelpSeeking'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    
    
-- select count(distinct participant_id) as freq, count(distinct session) from help_seeking
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		help_seeking
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1

-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'Evaluation'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'Evaluation'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    
    
-- select count(distinct participant_id) as freq, count(distinct session) from evaluation
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		evaluation
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1
-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'AssessingProgram'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'AssessingProgram'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    
    
-- select count(distinct participant_id) as freq, count(distinct session) from assessing_program
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		assessing_program
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1
-- ------------------------------------------------------------------------------------------------------------------------------------------------------
-- select count(distinct(study_id)) as freq,   count(distinct session_name)  from task_log where 
-- 	task_name = 'AssessingProgram'
--     and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 

-- SELECT
--     study_id, session_name, COUNT(*)
-- 	FROM
-- 		task_log
-- 	where 
-- 			task_name = 'AssessingProgram'
-- 			and study_id in (select id from calm.study where study_extension = 'TET' and id in (select study_id from participant where test_account = 0 and admin = 0)) 
-- 	GROUP BY
-- 		study_id, session_name
-- 	HAVING 
-- 		COUNT(*) > 1    
   
-- select count(distinct participant_id) as freq, count(distinct session) from assessing_program
-- 	where participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)

-- SELECT
--     participant_id, session, COUNT(*)
-- 	FROM
-- 		assessing_program
-- 	where 
-- 			participant_id in (select id from participant where study_id in (select id from study where study_extension = 'TET') and test_account = 0 and admin = 0)
-- 	GROUP BY
-- 		participant_id, session
-- 	HAVING 
-- 		COUNT(*) > 1
