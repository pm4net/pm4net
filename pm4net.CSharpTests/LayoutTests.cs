using OCEL.CSharp;
using pm4net.Algorithms.Layout;

namespace pm4net.CSharpTests
{
    public class LayoutTests
    {
        [Fact]
        public void CanFindGlobalRankGraphFromLog()
        {
            var json = File.ReadAllText("github_pm4py.jsonocel");
            var log = OcelJson.Deserialize(json, false);
            var globalOrder = StableGraphLayout.ComputeRankGraph(log);
            Assert.NotNull(globalOrder);
        }
    }
}