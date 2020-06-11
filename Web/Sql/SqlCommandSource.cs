// using System;
// using System.Collections.Generic;
// using Microsoft.FSharp.Core;
//
// namespace Services
// {
//     public class SqlCommandSource : Domain.Commands.ICommandSource
//     {
//         public async IAsyncEnumerable<Shared.CommandDto> Read()
//         {
//             Shared.CommandDto command = null;
//             
//             while (true)
//             {
//                 try
//                 {
//                     var line = await Console.In.ReadLineAsync();
//                     var chunks = line.Split(" ");
//                     var type = chunks[0];
//                     var body = chunks[1];
//                     command = new Shared.CommandDto(type, body);
//                 }
//                 catch (Exception e)
//                 {
//                     Console.WriteLine(e);
//                 }
//
//                 if (command != null)
//                 {
//                     yield return command;
//                 }
//             }
//         }
//
//         public void Reply<T>(string commandId, FSharpResult<T, string> result)
//         {
//             var message = result.IsOk ? result.ResultValue.ToString() : result.ErrorValue;
//             Console.WriteLine(message);
//         }
//     }
// }