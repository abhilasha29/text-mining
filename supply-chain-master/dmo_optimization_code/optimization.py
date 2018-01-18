'''

Problem:

Mr X. is planning a vacation and have planned to visit few nearby cities and their tourist places with his family.
According to his current plan, he will be able to spend only 1 day (12 hours) in singapore. As there are many interesting sight seeing options,
he let her wife and children to suggest max 10 places according to their preferences. He planned to rent a Car for the day and have prepared
 time matrix for each destinations suggested by his family.

His friend who is analyst by professional, offered to help in planned the day schedule, and intent to use optimization algorithm to solve the tour problem.

Table for minimum time spend at each location

Gardens by the Bay	120
Zoo	120
Botanic Gardens	60
National Museum	60
National Gallery	120
Universal Studios	240
Marina Bay Sands	30
Jurong Bird Park	60
Sentosa	120
Changi Museum	60

also,

Time Matrix (min) for Travelling
	Gardens by the Bay	Zoo	Botanic Gardens	National Museum	National Gallery	Universal Studios	Marina Bay Sands	Jurong Bird Park	Sentosa	Changi Museum
Gardens by the Bay	0	26	16	10	9	10	8	20	14	20
Zoo	26	0	18	24	26	28	26	22	30	24
Botanic Gardens	16	18	0	7	9	14	10	16	16	20
National Museum	10	24	7	0	3	10	4	16	12	18
National Gallery	9	26	9	3	0	10	4	16	12	16
Universal Studios	10	28	14	10	10	0	10	16	4	24
Marina Bay Sands	8	26	10	4	4	10	0	18	12	18
Jurong Bird Park	20	22	16	16	16	16	18	0	22	35
Sentosa	14	30	16	12	12	4	12	22	0	26
Changi Museum	20	24	20	18	16	24	18	35	26	0


Assume driving at 8am on Sat

'''

from __future__ import print_function
from ortools.constraint_solver import pywrapcp
from ortools.constraint_solver import routing_enums_pb2

# Travelling Time callback
class CreateTravellingTimeCallback(object):
    """Create callback to calculate travelling time between points."""

    def __init__(self):
        """Array of time taken between points to travel"""

        # travelling time in minutes
        self.time_matrix = [
            [0,  26, 16, 10, 9,	 10,  8, 20, 14, 20], # Gardens by the Bay
            [26, 0,	 18, 24, 26, 28,  2, 22, 30, 24], # Zoo
            [16, 18, 0,	 7,	 9,	 14,  1, 16, 16, 20], # Botanic Gardens
            [10, 24, 7,	 0,	 3,	 10,  4, 16, 12, 18], # National Museum
            [9,	 26, 9,	 3,	 0,	 10,  4, 16, 12, 16], # National Gallery
            [10, 28, 14, 10, 10,  0,  1, 16, 4,	 24], # Universal Studios
            [8,	 26, 10, 4,	 4,	 10,  0, 18, 12, 18], # Marina Bay Sands
            [20, 22, 16, 16, 16, 16,  18, 0, 22, 35], # Jurong Bird Park
            [14, 30, 16, 12, 12, 4,	  12, 22, 0, 26], # Sentosa
            [20, 24, 20, 18, 16, 24,  18, 35, 26, 0], # Changi Museum
        ]

    def TravelTime(self,from_node, to_node):
        return self.time_matrix[from_node][to_node]

# Time Spend at location Callback (time in minutes)
class CreateTimeSpendAtLocationCallback(object):
    """Create callback to get time to be spent at each location"""

    def __init__(self, durations):
        self.matrix = durations

    def ExplorationTime(self, node):
        return self.matrix[node]

class CreateMoneySpendAtLocationCallback(object):
    """Create callback to get time to be spent at each location"""

    def __init__(self, locations):
        self.matrix = locations

    def Cost(self, node,second):
        return self.matrix[node]

class CreateTotalTimeCallback(object):
    """Create callback to get total times between locations"""

    def __init__(self,exploration_time_callback, travel_time_callback):
        self.exploration_time_callback = exploration_time_callback
        self.travel_time_callback = travel_time_callback

    def TotalTime(self, from_node, to_node):
        exploration_time = self.exploration_time_callback(from_node)
        travel_time = self.travel_time_callback(from_node, to_node)
        return exploration_time + travel_time

def main():

    # locations
    location_names = ['Gardens by the Bay','Zoo','Botanic Gardens','National Museum','National Gallery','Universal Studios',
                        'Marina Bay Sands','Jurong Bird Park','Sentosa','Changi Museum']

    # weights (time in minutes)
    location_weights = [120,120,60,60,120,240,30,60,120,60] # minimum time required to spend at each location
    location_ticket_costs = [30,66,30,10,10,80,1,20,50,10]

    l_size = len(location_names)
    # The number of routes
    num_routes = 2

    # Nodes are indexed from 0 to l_size - 1.
    # The depot is the stating node of the route

    depot = 6


    # Creating model
    if l_size > 0:
        routing = pywrapcp.RoutingModel(l_size,num_routes,depot)
        search_parameters  = pywrapcp.RoutingModel.DefaultSearchParameters()

        search_parameters.first_solution_strategy = (routing_enums_pb2.FirstSolutionStrategy.PATH_CHEAPEST_ARC)

        exploration_times = CreateTimeSpendAtLocationCallback(location_weights)
        exploration_time_callback = exploration_times.ExplorationTime

        travel_times = CreateTravellingTimeCallback()
        travel_time_callback = travel_times.TravelTime

        total_times = CreateTotalTimeCallback(exploration_time_callback,travel_time_callback)
        total_time_callback = total_times.TotalTime

        money_costs = CreateMoneySpendAtLocationCallback(location_ticket_costs)
        money_cost_callback = money_costs.Cost

        # time dimension , minutes in scale, max available time 12hrs
        horizon = 12 * 60 *1 # 12hrs * 60 minutes
        fix_start_cumul_to_zero = False
        time = "Time"
        money = "Money"

        routing.SetArcCostEvaluatorOfAllVehicles(travel_time_callback)
        #routing.SetFixedCostOfAllVehicles(horizon)


        routing.AddDimension(total_time_callback,horizon,horizon,fix_start_cumul_to_zero, time)

        max_budget = 1000
        routing.AddDimension(money_cost_callback, max_budget, max_budget, fix_start_cumul_to_zero, money)
        routing.AddVariableMinimizedByFinalizer(routing.CumulVar(0, money));

        #  add cost constrains - TBD

        print("trying to find solution", routing.GetCostClassesCount())
        # Solve and print solutions if any
        possible_plan = routing.SolveWithParameters(search_parameters)

        if possible_plan:
            size = len(location_names)

            # Solution Cost
            print("Total time of all paths: " + str(possible_plan.ObjectiveValue()) + "\n")

            # Inspect Solution
            time_dimension = routing.GetDimensionOrDie(time)
            cost_dimension = routing.GetDimensionOrDie(money)

            for location in range(size):
                index = routing.Start(location)
                plan_output = 'Route {0}: '.format(location)

                while not routing.IsEnd(index):
                    node_index = routing.IndexToNode(index)
                    time_var = time_dimension.CumulVar(index)
                    cost_var = cost_dimension.CumulVar(index)
                    plan_output += \
                                "{node_index} Time({tmin},{tmax}) Cost({cmin},{cmax}) -> ".format(
                                    node_index=location_names[node_index],
                                    tmin=str(possible_plan.Min(time_var)),
                                    tmax=str(possible_plan.Max(time_var)),
                                    cmin=str(possible_plan.Min(cost_var)),
                                    cmax=str(possible_plan.Min(cost_var)),
                                )
                    index = possible_plan.Value(routing.NextVar(index))

                node_index = routing.IndexToNode(index)
                time_var = time_dimension.CumulVar(index)
                plan_output += \
                                "{node_index} Time({tmin},{tmax}) -> ".format(
                                    node_index=location_names[node_index],
                                    tmin=str(possible_plan.Min(time_var)),
                                    tmax=str(possible_plan.Max(time_var))
                                )

                print(plan_output)
                print("\n")

        else:
            print("No Solution found")

if __name__ == '__main__':
  main()