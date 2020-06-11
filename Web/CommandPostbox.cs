using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using Command = global::Shared.CommandDto;

namespace Web
{
    public class CommandPostbox
    {
        private readonly BufferBlock<(Command, TaskCompletionSource<string>)> _buffer;

        public CommandPostbox()
        {
            _buffer = new BufferBlock<(Command, TaskCompletionSource<string>)>();
        }
        
        public Task<string> Post(Command message)
        {
            var completionSource = new TaskCompletionSource<string>();
            _buffer.Post((message, completionSource));
            return completionSource.Task;
        }
        
        public async IAsyncEnumerable<(Command, Action<string>)> Read([EnumeratorCancellation] CancellationToken cancellationToken)
        {
            while (!cancellationToken.IsCancellationRequested)
            {
                var (message, taskCompletionSource) = await _buffer.ReceiveAsync(cancellationToken);
                yield return (message, taskCompletionSource.SetResult);
            }
        }
    }
}