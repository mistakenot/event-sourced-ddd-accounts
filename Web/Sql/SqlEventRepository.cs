// using System;
// using System.Collections.Generic;
// using System.Data;
// using System.Threading.Tasks;
// using Dapper;
//
// namespace Services
// {
//     public class SqlEventRepository : Domain.StateModule.IEventRepository
//     {
//         private readonly IDbConnection _dbConnection;
//
//         public SqlEventRepository(IDbConnection dbConnection)
//         {
//             _dbConnection = dbConnection ?? throw new ArgumentNullException(nameof(dbConnection));
//         }
//         
//         public async Task<Domain.StateModule.StateSaveResult> Save(Shared.EventDto eventDto)
//         {
//             await _dbConnection.ExecuteAsync("");
//             return new Domain.StateModule.StateSaveResult(1);
//         }
//
//         public async IAsyncEnumerable<Shared.EventDto> Read(int sequenceId)
//         {
//             var results = await _dbConnection
//                 .QueryAsync<Shared.EventDto>(
//                 "select id as Id, type as Type, body as Body from event where id >= @sequenceId",
//                 new {sequenceId});
//
//             foreach (var eventDto in results)
//             {
//                 yield return eventDto;
//             }
//         }
//     }
// }
