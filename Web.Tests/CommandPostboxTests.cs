using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Xunit;
using Xunit.Abstractions;
using CommandDto = global::Shared.CommandDto;

namespace Web.Tests
{
    public class CommandPostboxTests
    {
        private readonly ITestOutputHelper _testOutputHelper;

        public CommandPostboxTests(ITestOutputHelper testOutputHelper)
        {
            _testOutputHelper = testOutputHelper;
        }

        [Fact(Timeout = 1000)]
        public async Task Replies_Ok()
        {
            var postbox = new CommandPostbox();
            var command = new CommandDto("123", "");
            var postTask = postbox.Post(command);
            var tokenSource = new CancellationTokenSource();
            
            async Task<string> Process()
            {
                await foreach (var (c, reply) in postbox.Read(tokenSource.Token))
                {
                    reply(c.Type);
                    return c.Body;
                }

                return "";
            }
            
            var results = await Task.WhenAll(postTask, Process());
            Assert.Equal("123", results[0]);
        }
        
        [Fact(Timeout = 1000)]
        public async Task Buffers_Ok()
        {
            var postbox = new CommandPostbox();
            var commands = Enumerable
                .Range(0, 5)
                .Select(i => new CommandDto(i.ToString(), ""));

            async Task Post()
            {
                foreach (var command in commands)
                {
                    var reply = await postbox.Post(command);
                    Assert.Equal(reply, command.Type);
                }
            }
            
            var tokenSource = new CancellationTokenSource();
            
            async Task Process()
            {
                await foreach (var (c, reply) in postbox.Read(tokenSource.Token))
                {
                    reply(c.Type);

                    if (c.Type == "4")
                    {
                        return;
                    }
                }
            }
            
            await Task.WhenAll(Post(), Process());
        }
    }
}
