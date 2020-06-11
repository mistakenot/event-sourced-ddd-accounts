using System;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using State = Domain.State;

namespace Web
{
    public interface IStateRepository
    {
        Task<State> Get();
        Task Set(State state);
    }

    public class MemoryStateRepository : IStateRepository
    {
        private readonly ILogger<MemoryStateRepository> _logger;
        private State _state = Domain.StateModule.empty;

        public MemoryStateRepository(ILogger<MemoryStateRepository> logger)
        {
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }
        
        public Task<State> Get() => Task.FromResult(_state);

        public Task Set(Domain.State state)
        {
            _state = state;
            _logger.LogInformation(_state.ToString());
            return Task.CompletedTask;
        }
    }
}