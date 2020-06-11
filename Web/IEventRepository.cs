using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using EventDto = Shared.EventDto;

namespace Web
{
    public interface IEventRepository
    {
        Task<int> Save(EventDto eventDto);
        IAsyncEnumerable<EventDto> ReadFrom(int id, CancellationToken cancellationToken);
    }
    
    public class MemoryEventRepository : IEventRepository
    {
        private readonly ICollection<EventDto> _events = new List<EventDto>();
        
        public Task<int> Save(EventDto eventDto)
        {
            _events.Add(eventDto);
            return Task.FromResult(eventDto.Id);
        }

        public async IAsyncEnumerable<EventDto> ReadFrom(int id, [EnumeratorCancellation] CancellationToken cancellationToken)
        {
            foreach (var eventDto in _events)
            {
                yield return eventDto;
            }
        }
    }
}