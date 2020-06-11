using System;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

namespace Web
{
    public class CommandService : BackgroundService
    {
        private readonly CommandPostbox _postbox;
        private readonly IEventRepository _eventRepository;
        private readonly IStateRepository _stateRepository;
        private readonly ILogger<CommandService> _logger;
        private Domain.State _state;

        public CommandService(
            CommandPostbox postbox,
            IEventRepository repository,
            IStateRepository stateRepository,
            ILogger<CommandService> logger)
        {
            _postbox = postbox ?? throw new ArgumentNullException(nameof(postbox));
            _eventRepository = repository ?? throw new ArgumentNullException(nameof(repository));
            _stateRepository = stateRepository ?? throw new ArgumentNullException(nameof(stateRepository));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
            _state = Domain.StateModule.empty;
        }

        protected override async Task ExecuteAsync(CancellationToken stoppingToken)
        {
            _logger.LogInformation("Refreshing state");

            await foreach (var eventDto in _eventRepository.ReadFrom(0, stoppingToken))
            {
                _state = Domain.StateModule.reducer(_state, eventDto);
            }
            
            _logger.LogInformation("State refresh complete");

            await foreach (var (command, reply) in _postbox.Read(stoppingToken))
            {
                var result = await Process(command);
                reply(result);
            }
        }

        private async Task<string> Process(global::Shared.CommandDto command)
        {
            _logger.LogInformation("Received {Type}", command.Type);

            try
            {
                var result = Domain.processCommand(_state, command);

                _logger.LogInformation("Processed ok: {IsOk}", result.IsOk);

                if (result.IsOk)
                {
                    var (newState, eventDto) = result.ResultValue;
                    await _eventRepository.Save(eventDto);
                    await _stateRepository.Set(newState);
                    _state = newState;
                    _logger.LogInformation("Updated state");
                }

                return result.IsOk ? result.ResultValue.Item2.Id.ToString() : result.ErrorValue;
            }
            catch (Exception e)
            {
                _logger.LogError(e.ToString());
                return e.Message;
            }
        }
    }
}