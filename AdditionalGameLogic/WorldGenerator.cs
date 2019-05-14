using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AdditionalGameLogic
{
    public class WorldGenerator
    {

        public static string Foo(Model.InGameMessage msg)
        {
            if (msg.IsUpdateTick)
            {
                var updateTickMsg = msg.updateTick;

                return updateTickMsg.ToString();
            }
            return "FOO";
        }
    }
}
