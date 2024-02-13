UPDATE [MA_SDT_NS_DATA].[dbo].[publicationTrackingProduction]
SET [g6] = 'Tim Hogan'
WHERE [publication] = 'Graduate labour market statistics';


SELECT * FROM [MA_SDT_NS_DATA].[dbo].[publicationTrackingProduction]
WHERE [g6] in ('TBA', 'TBC');