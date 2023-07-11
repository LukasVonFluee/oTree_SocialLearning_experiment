from otree.api import *
import itertools
import random
import numpy
""" Notice, 'import...' is greyed out as long as the package you want to import is not yet in use in your code. 
As soon as you use functions, methods, etc, from the package, it will change."""
author = 'Lukas von Fluee'
doc = """
Social learning task, with 4 groups; 5 triangle individual learners, 5 square individual learners,
undefined number of triangle social learners, undefined number of square social learners.
"""


"""Below function is for likert scale questionnaire (needs to be defined before Player class where I define questionnaire fields):
(see https://otree.readthedocs.io/en/latest/misc/tips_and_tricks.html?highlight=make_field#how-to-make-many-fields)"""


def make_field(label):
    return models.IntegerField(
        choices=[1,2,3,4,5,6,7],
        label=label,
        widget=widgets.RadioSelect,
    )

############################################################################################################
############################################################################################################
############################################################################################################
#                                                 CLASSES                                                  #
############################################################################################################
############################################################################################################
############################################################################################################


############################################################################################################
#                                                 CONSTANTS                                                #
############################################################################################################
class C(BaseConstants):
    NAME_IN_URL = 'urn_choice_experiment'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 80
    NUM_ROUNDS_BLOCK = 4
    ########################################################################################################
    #                                        TYPE AND GROUP CONSTANTS                                      #
    ########################################################################################################
    """Types: There are two types; A (individual learners), and B (social learners).
        Create constants consisting of strings to use in 'creating subsession:'"""
    TYPE_A = 'individual_learner'
    TYPE_B = 'social_learner'
    """Names of these types to show on html pages (we don't use the terms 'individual' and 'social learners' to show to
    participants to avoid any unwanted associations and demand effects:"""
    TYPE_A_NAME = 'A'
    TYPE_B_NAME = 'B'
    """Groups: There are two groups; 'triangle', and 'square':'"""
    GROUP_TRIANGLE = 'triangle'
    GROUP_SQUARE = 'square'
    ########################################################################################################
    #                                          QUIZ ANSWERS                                                #
    ########################################################################################################
    """Quiz Answers:"""
    QUIZ_ANSWER_1 = 3
    QUIZ_ANSWER_2 = 3
    QUIZ_ANSWER_3 = 2
    QUIZ_TYPE_A_ANSWER_4 = 4
    QUIZ_TYPE_B_ANSWER_4 = 3
    QUIZ_TYPE_A_ANSWER_5 = 75
    QUIZ_TYPE_B_ANSWER_5 = 2
    QUIZ_TYPE_A_ANSWER_6 = 25
    QUIZ_TYPE_B_ANSWER_6 = 0
    QUIZ_TYPE_B_ANSWER_7 = 1


############################################################################################################
#                                                 SUBSESSION                                               #
############################################################################################################
class Subsession(BaseSubsession):
    red_majority = models.IntegerField()
    number_left_urn_triangle = models.IntegerField()
    number_left_urn_square = models.IntegerField()
    length_multi_digit = models.IntegerField()
    timeout_seconds_multi_digit = models.IntegerField()


############################################################################################################
#                                                   GROUP                                                  #
############################################################################################################
class Group(BaseGroup):
    pass


############################################################################################################
#                                                   PLAYER                                                 #
############################################################################################################
class Player(BasePlayer):
    monetary_earnings = models.CurrencyField()
    ########################################################################################################
    #                                               CONSENT                                                #
    ########################################################################################################
    """On the first page 'Consent' we ask for participation consent;
     Consent is a boolean; Participant agrees = 1, participant disagrees = 0.
     Depending on whether they agree or disagree, they will reach a different next page.
     Participants who disagree will reach the page 'ConsentDisagree' which is the last page for them."""
    consent = models.BooleanField()
    ########################################################################################################
    #                                          INSTRUCTIONS ORDER                                          #
    ########################################################################################################
    """The following variable is only relevant for the instructions. It concerns the order in which Type B participants
    are shown the example compositions of the urns where I explain to them that everyone faces the same two urns.
    My supervisors suggested that it makes sense to randomize this order on the participant level to avoid
    any potential anchor effect. I assign values to this field in 'creating_session'. """
    order_same_urns = models.BooleanField()
    ########################################################################################################
    #                                              URN CHOICE                                              #
    ########################################################################################################
    """Urn choice field to register urn choices on the player/participant level:"""
    urn_choice = models.BooleanField(required=True, default=None)
    ########################################################################################################
    #                                             OPTIMAL URN                                              #
    ########################################################################################################
    """Each participant gets a field showing whether their optimal urn (i.e. the one containing 3/4 marbles 
     with their winning color) is the LEFT (=0) or RIGHT (=1) urn:"""
    optimal_urn = models.IntegerField()
    ########################################################################################################
    #                                        COLOR OF THE DRAWN marbleS                                      #
    ########################################################################################################
    color_drawn_marble_left = models.StringField()  # this is for example block in instructions
    color_drawn_marble_right = models.StringField()  # this is for example block in instructions
    color_drawn_marble = models.StringField()  # this is for actual experiment
    ########################################################################################################
    #                                      MEMORIZE MULTI-DIGIT NUMBER                                     #
    ########################################################################################################
    """Only relevant for type B participants with treatment cognitive load = 1:"""
    memorize_number = models.StringField()  # This is used to assign individually generated random numbers
    insert_memorize_number = models.StringField(blank=True)  # this is the field to register the numbers that participant enter
    """I have to clean the player field insert_memorize_number every time participants enter a number because
    otherwise it stores the number they entered as default and shows it to them next time they have to enter
    a number -> don't know how to solve this currently. And so, as a solution I create an additional player
    field where I store the number they entered in the following variable:"""
    recalled_number = models.StringField(blank=True)
    memorized_correctly = models.BooleanField(default=None)  # this is to register whether a participant has memorized the number correctly
    ########################################################################################################
    #                                               TREATMENTS                                             #
    ########################################################################################################
    """The first field here called "treatment_assignment" is only needed temporarily to assign treatment sequences. 
    Then, based on this sequence, players will be assigned values for their treatment fields "similar_ingroup" and "observing_ingroup"."""
    treatment_assignment = models.IntegerField()
    cognitive_load = models.IntegerField()  # I assign values in 'creating_session' function
    digits_number = models.IntegerField()  # We decided to vary this within session between participants with cog. load.
    similar_ingroup = models.FloatField()  # I assign values in 'creating_session' function
    observing_ingroup = models.BooleanField()  # I assign values in 'creating_session' function
    ########################################################################################################
    #                              INCENTIVE STRUCTURE SOCIAL LEARNERS                                     #
    ########################################################################################################
    """Social learners in the cognitive load treatment can either earn points based on memorizing task or 
    social learning task. There will be a 50% probability for each incentive structure and this will be 
    assigned at the beginning of the experiment. I.e., every social learner in the cognitive load treatment
    will receive a boolean value 0/1 which then decides whether they receive points for memorizing task or 
    for social learning task over the course of all 20/20 blocks. Social learners will not know for which 
    task they will get paid for. Importantly, the value = 's' refers to getting paid for the social learning
    task, and the value = 'm' refers to getting paid for memorizing. If a given social learner gets paid 
    for the memorzing task, then 4 marbles are drawn from optimal urn if memorized correctly and vice versa."""
    pay_task = models.StringField()
    ########################################################################################################
    #                                 SHARING WINNING COLOR WITH INGROUP                                   #
    ########################################################################################################
    """Social learners can have a high or low similarity to ingroup in terms of winning points for the same 
    color of marbles. similar_ingroup can either be 0.1 or 0.9. This indicates a probability which means that
    in a given block, a social learner can share the winning color with their ingroup or not. What's the 
    case in a given block will be registered in the following player field called 'share_winning_color'
    which, if = 0, means that a focal social learner does not share the winning color with their ingroup
    and vice versa."""
    share_winning_color = models.IntegerField()
    ########################################################################################################
    #                                      TYPE AND GROUP ASSIGNMENT                                       #
    ########################################################################################################
    """Note: 'type' has already a function associated with it and thus, it's better to use a different term:"""
    type_assignment = models.StringField()
    """Note:'group' has already a function associated with it and thus, it's better to use a different term:"""
    group_assignment = models.StringField()
    ########################################################################################################
    #                                            QUIZ QUESTIONS                                            #
    ########################################################################################################
    """The following are used to measure whether a participant has initially answered the question incorrectly:"""
    wrong_answer_1 = models.BooleanField()
    wrong_answer_2 = models.BooleanField()
    wrong_answer_3 = models.BooleanField()
    wrong_answer_4 = models.BooleanField()
    wrong_answer_5 = models.BooleanField()
    wrong_answer_6 = models.BooleanField()
    wrong_answer_7 = models.BooleanField()

    """Below I define the quiz questions. All 9 of them have to be answered by type B participants. Only 
    part of them have to be answered by Type A participants."""

    question1 = models.IntegerField(
        label=''
    )

    question2 = models.IntegerField(
        label=''
    )

    question3 = models.IntegerField(
        choices=[
            [1, 'a) My group membership and my type assignment can change after 10 blocks.'],
            [
                2,
                'b) My group membership and my type assignment does not change throughout the whole experiment.',
            ],
            [3, 'c) My group membership and my type assignment can change after every block.'],
            [4, 'd) My group membership cannot change but my type assignment can change.'],
        ],
        widget=widgets.RadioSelect,
        label="",
    )

    question4_type_a = models.IntegerField(
        label=''
    )

    question4_type_b = models.IntegerField(
        choices=[
            [
                1,
                'a) Yes, I always get points for the same color as type A participants of my own group.',
            ],
            [
                2,
                'b) No, I never get points for the same color as type A participants of my own group.',
            ],
            [
                3,
                'c) No, I get points for the same color of marbles as type A participants of my own group with a '
                'probability of either 10% or 90% in a given block.',
            ],
            [
                4,
                'd) No, I get points for the same color of marbles as type A participants of my own group with a '
                'probability of either 20% or 80% in a given block.',
            ],
        ],
        widget=widgets.RadioSelect,
        label="",
    )

    question5_type_a = models.IntegerField(
        label=''
    )

    question5_type_b = models.IntegerField(
        choices=[
            [
                1,
                'a) Type A participants of both groups, the triangle and the square group, receive points for blue.',
            ],
            [
                2,
                'b) Type A participants of the triangle group and type A participants of the square group receive '
                'points for opposite colors.',
            ],
            [
                3,
                'c) Type A participants of both groups, the triangle and the square group, receive points for red.',
            ],
            [
                4,
                'd) The winning color for Type A participants of the two groups changes between blocks.',
            ],
        ],
        widget=widgets.RadioSelect,
        label="",
    )

    question6_type_a = models.IntegerField(
        label=''
    )

    question6_type_b = models.IntegerField(
        label=''
    )

    question7_type_b = models.IntegerField(
        choices=[
            [
                1,
                'a) The right urn is optimal for type A participants of the square group.',
            ],
            [
                2,
                'b) We cannot know which urn is optimal for type A participants of the square group just because we know which urn is '
                'optimal for type A participants of the triangle group.',
            ],
            [
                3,
                'c) With a probability of 50% the left urn is optimal and with a probability of 50% the right urn is optimal'
                'for type A participants of the square group.',
            ],
            [
                4,
                'd) The left urn is optimal for type A participants of the square group.',
            ],
        ],
        widget=widgets.RadioSelect,
        label="",
    )

    ########################################################################################################
    #                                      POST EXPERIMENT QUESTIONNAIRE                                   #
    ########################################################################################################
    age = models.IntegerField(label='What is your age?', min=18, max=99)
    gender = models.StringField(
        choices=[['Man', 'Man'], ['Woman', 'Woman'], ['Other', 'Other']],
        label='What is your gender?',
        widget=widgets.RadioSelect,
    )
    study_subject = models.StringField(
        label='What is your field of study?'
    )
    # Question 1
    q1 = make_field('When choosing between the two urns, did you spend very little time on your choice or a lot of time '
                    '(1=very little time, 7=a lot of time)?')
    # Question 2
    q2 = make_field('How difficult was it for you to decide between the two urns (1=not difficult, 7=very difficult)?')
    # Question 3
    q3 = make_field('When progressing through the 20 blocks, did it get harder to choose between the two urns or easier or did the '
                    'difficulty not change at all (1=it got easier, 4=no change, 7=it got harder)?')
    # Question q4nocl
    q4nocl = make_field('How distracting was it that we showed you the multi digit number every time before showing you the choices of '
                        'type A participants (1=not distracting, 7=very distracting)?')
    # Question 4
    q4 = make_field('How distracting was the memorization task (1=not distracting, 7=very distracting)?')
    # Question q5nocl
    q5nocl = make_field('Did you try to memorize the number? (1=never, 7=every time)?')
    # Question 5
    q5 = models.IntegerField(
        min=0, max=20,
        label="How many times do you think you have memorized the number correctly? (please write a number between 0-20 in the box below)",
        )
    # Question 6
    q6 = models.IntegerField(
        min=0, max=100,
        label="How did you allocate your cognitive resources between the memorization task and the urn choice task "
              "(0=all focus on memorizing, 100=all focus on urn choice task)?",
    )
    # Question 7
    q7 = make_field('How difficult was it for you to recall the multi digit numbers (1=not difficult, 7=very difficult)?')
    # Question 8
    q8 = make_field('When progressing through the 20 blocks, did it get harder to memorize the multi digit number or easier or did the '
                    'difficulty not change at all (1=it got easier, 4=no change, 7=it got harder)?')


############################################################################################################
############################################################################################################
############################################################################################################
#                                               FUNCTIONS                                                  #
############################################################################################################
############################################################################################################
############################################################################################################


############################################################################################################
############################################################################################################
#                                            SESSION FUNCTION                                              #
############################################################################################################
############################################################################################################
def creating_session(subsession: Subsession):
    import itertools
    """This function will automatically be executed when a new session is started"""
    if subsession.round_number == 1:
        """Print round number"""
        print('round number:', subsession.round_number)
        ############################################################################################################
        ############################################################################################################
        #                                      CREATE LIST WITH BLOCK NUMBERS                                      #
        ############################################################################################################
        ############################################################################################################
        block_number = subsession.session.config['block_number']
        subsession.session.block_number = block_number
        print(
            'This is just to prove that I managed to access the empty list "block_number=[]" created in SESSION_CONFIGS and store it'
            'under a session variable now:',
            subsession.session.block_number,
        )
        ############################################################################################################
        ############################################################################################################
        #                                      CREATE LIST WITH SHUFFLED URNS                                      #
        ############################################################################################################
        ############################################################################################################
        import random
        red_marbles_majority = subsession.session.config['red_marbles_majority']
        subsession.session.red_marbles_majority = red_marbles_majority
        print(
            'This is just to prove that I managed to access the empty list "red_marbles_majority=[]" created in '
            'SESSION_CONFIGS and store it under a session variable now:',
            subsession.session.red_marbles_majority,
        )
        num_rounds_total = subsession.session.config['num_rounds_total']
        print('test whether I can access num_rounds_total:', num_rounds_total)

        num_rounds_per_block = subsession.session.config['num_rounds_per_block']
        print('test whether I can access num_rounds_per_block:', num_rounds_per_block)

        n_blocks = int(num_rounds_total / num_rounds_per_block)
        print('n_blocks:', n_blocks)
        """Generate vector with urn shuffling. In particular, I create a list of randomly selected 0s and 1s (with equal probability) to
        determine for every of the 20 blocks the composition of the two sets of marbles. 0 means that there is a majority of red marbles in the 
        LEFT urn and 1 means that there is a majority of red marbles in the RIGHT urn. I append the above created session variable called
        subsession.session.red_marbles_majority."""
        urn_shuffling_results = [None] * n_blocks
        for block in range(0, n_blocks):
            urn_shuffling_results[block] = random.randint(0, 1)
            subsession.session.red_marbles_majority.append(urn_shuffling_results[block])
        print('urn_shuffling_results', urn_shuffling_results)
        print('We store this now as a session variable. Note, if red_marbles_majority=0, then there is a majority'
              'of red marbles (3/4) in the left urn and if red_marbles_majority=1 its the opposite. To check whether'
              'we were able to store the variable, lets print it subsession.session.red_marbles_majority:',
              subsession.session.red_marbles_majority)
        ############################################################################################################
        ############################################################################################################
        #                  ACCESS OTHER SESSIONS_CONFIGS VARIABLES AND STORE THEM UNDER SESSION VARIABLES          #
        ############################################################################################################
        ############################################################################################################
        """Get winning color for triangle from SESSION_CONFIGS in settings.py and store it in a session variable.
            This is a parameter that we might change across sessions. We can always access the variable value by
            typing 'subsession.session.winning_color_triangle' or on templates with either;
            {{ session.config.winning_color_triangle }} or; {{ session.winning_color_triangle }},
            e.g. if we want to show certain images, based on the condition whether the winning color for triangle is red or
            blue (note, the winning color for square is always the opposite, i.e. if the winning color for triangle is red,
            the winning color for square is blue."""
        winning_color_triangle = subsession.session.config['winning_color_triangle']
        subsession.session.vars['winning_color_triangle'] = winning_color_triangle
        print(
            'Winning color of triangle type A participants:',
            subsession.session.vars['winning_color_triangle'],
        )
        winning_color_square = 'blue' if winning_color_triangle == 'red' else 'red'
        subsession.session.vars['winning_color_square'] = winning_color_square
        print(
            'Winning color of square type A participants:',
            subsession.session.vars['winning_color_square'],
        )
        """We have defined the length of the multi-digit number that the social learners in the cognitive load treatment have to memorize.
        Like we did above for the winning color, we store this length of the multi-digit number now as a session variable:"""
        length_multi_digit = subsession.session.config['length_multi_digit']
        subsession.session.length_multi_digit = length_multi_digit
        print(
            'The length of the multi-digit number that social learners in the cognitive load treatment have to memorize in this session is:',
            subsession.session.length_multi_digit,
        )
        """Store number of individual learners (Type A) by accessing it in settings.py under SESSION_CONFIGS:"""
        nbr_individual_learners = subsession.session.config['nbr_individual_learners']
        subsession.session.nbr_individual_learners = nbr_individual_learners
        print('number of individual learners:', subsession.session.nbr_individual_learners)
        """Now I define what should only happen in the first round of this app. This consists of assigning group, type,
            and treatments. And because I will store it under participant variables, I can access these values throughout
            the whole session (i.e. across multiple apps):"""
        ############################################################################################################
        ############################################################################################################
        #                                       TYPE AND GROUP ASSIGNMENTS                                         #
        ############################################################################################################
        ############################################################################################################
        """Type Assignment:"""
        """Note, I only need 10 individual learners (5 in the triangle group and 5 in the square group). I stored the number of individual
        learners that we use in the experiment in the SESSION_CONFIGS in settings.py file and I stored this number further above under the 
        variable name 'nbr_individual_learners':"""
        for player in subsession.get_players():
            print('player id:', player.id)
            participant = player.participant
            if participant.id_in_session <= nbr_individual_learners:
                player.type_assignment = C.TYPE_A
                participant.type_assignment = player.type_assignment
            else:
                player.type_assignment = C.TYPE_B
                participant.type_assignment = player.type_assignment
        """Create list with all Type_A players"""
        type_a_participants = [p for p in subsession.get_players() if p.participant.type_assignment == C.TYPE_A]
        """Create list with all Type_B players"""
        type_b_participants = [p for p in subsession.get_players() if p.participant.type_assignment == C.TYPE_B]
        """Group Assignment:"""
        """First within type A participants:"""
        groups_for_type_a = itertools.cycle([C.GROUP_TRIANGLE, C.GROUP_SQUARE])
        for player in type_a_participants:
            participant = player.participant
            player.group_assignment = next(groups_for_type_a)
            participant.group_assignment = player.group_assignment
        """Now within type B participants:"""
        groups_for_type_b = itertools.cycle([C.GROUP_TRIANGLE, C.GROUP_SQUARE])
        for player in type_b_participants:
            participant = player.participant
            player.group_assignment = next(groups_for_type_b)
            participant.group_assignment = player.group_assignment
        """Create list with all Type_B players of group C.GROUP_TRIANGLE"""
        type_b_triangle_participants = [p for p in subsession.get_players() if p.participant.type_assignment == C.TYPE_B and p.participant.group_assignment == C.GROUP_TRIANGLE]
        """Create list with all Type_B players of group C.GROUP_SQUARE"""
        type_b_square_participants = [p for p in subsession.get_players() if p.participant.type_assignment == C.TYPE_B and p.participant.group_assignment == C.GROUP_SQUARE]
        ############################################################################################################
        ############################################################################################################
        #                                            ORDER SAME URNS                                               #
        ############################################################################################################
        ############################################################################################################
        """The following is only relevant for the instructions. It concerns the order in which Type B participants
        are shown the example compositions of the urns where I explain to them that everyone faces the same two urns.
        My supervisors suggested that it makes sense to randomize this order on the participant level to avoid
        any potential anchor effect."""
        order_same_urns_left_urn_red_majority = itertools.cycle([0, 1])
        for player in type_b_participants:
            player.order_same_urns = next(order_same_urns_left_urn_red_majority)
        ############################################################################################################
        ############################################################################################################
        #                                           TREATMENT ASSIGNMENTS                                          #
        ############################################################################################################
        ############################################################################################################

        ############################################################################################################
        #                                              COGNITIVE LOAD                                              #
        ############################################################################################################
        """ Now assign players randomly to treatments. Do it balanced with 'itertools', so that an equal amount of 
        players get assigned to all treatment combinations.
        IMPORTANT: Save the treatment on the participant level so that we can access it throughout the session."""
        """1. Random assignment of between-subjects treatment cognitive load, with 0 = 'no cognitive load' and 
        1 = 'cognitive load'. To not have all square participants have cogn. load value = 0 or opposite and same 
        for triangle (which would be the case if I simply use itertools applied to type_b_participants because I 
        applied the group assignment to type_b_participants as well, and thus it would create the exact same 
        sequence of 0/1, i.e. either all square would have cogn. load = 0 or 1 and opposite for triangle), I will 
        apply it within the two groups, triangle and square:"""
        treatment_cognitive_load = itertools.cycle([0, 1])
        for player in type_b_triangle_participants:
            participant = player.participant
            player.cognitive_load = next(treatment_cognitive_load)
            participant.cognitive_load = player.cognitive_load

        treatment_cognitive_load = itertools.cycle([0, 1])
        for player in type_b_square_participants:
            participant = player.participant
            player.cognitive_load = next(treatment_cognitive_load)
            participant.cognitive_load = player.cognitive_load

        """To have balanced treatment assignment within the different cognitive load treatments, I need to first create
        two lists including the participants with cognitive_load = 1 and cognitive_load = 0. Then within these two 
        groups, I will do the treatment assignment of the similar_ingroup treatment."""
        """Create list with all players with participant.cognitive_load=0"""
        cognitive_load_0_participants = [p for p in type_b_participants if p.participant.cognitive_load == 0]
        """Create list with all players with participant.cognitive_load=1"""
        cognitive_load_1_participants = [p for p in type_b_participants if p.participant.cognitive_load == 1]
        ############################################################################################################
        #                                          INCENTIVE STRUCTURE                                             #
        ############################################################################################################
        """Within the group of social learners under cognitive load, determine who will get paid for memorizing task
        and who will get paid for social learning task. For that I first create four groups; cl=0 square, cl=0 triangle,
        cl=1 square, cl=1 triangle, so that I can randomize the incentive structure across all those groups evenly."""
        cognitive_load_0_participants_triangle = [p for p in type_b_participants if p.participant.cognitive_load == 0 and p.participant.group_assignment == C.GROUP_TRIANGLE]
        cognitive_load_0_participants_square = [p for p in type_b_participants if p.participant.cognitive_load == 0 and p.participant.group_assignment == C.GROUP_SQUARE]
        cognitive_load_1_participants_triangle = [p for p in type_b_participants if p.participant.cognitive_load == 1 and p.participant.group_assignment == C.GROUP_TRIANGLE]
        cognitive_load_1_participants_square = [p for p in type_b_participants if p.participant.cognitive_load == 1 and p.participant.group_assignment == C.GROUP_SQUARE]
        incentive_structure = itertools.cycle(['s', 'm'])
        for player in cognitive_load_0_participants_triangle:
            participant = player.participant
            player.pay_task = next(incentive_structure)
            participant.pay_task = player.pay_task
        for player in cognitive_load_0_participants_square:
            participant = player.participant
            player.pay_task = next(incentive_structure)
            participant.pay_task = player.pay_task
        for player in cognitive_load_1_participants_triangle:
            participant = player.participant
            player.pay_task = next(incentive_structure)
            participant.pay_task = player.pay_task
        for player in cognitive_load_1_participants_square:
            participant = player.participant
            player.pay_task = next(incentive_structure)
            participant.pay_task = player.pay_task
        ############################################################################################################
        #                                           PERMUTATION NUMBER                                             #
        ############################################################################################################
        """We decided to do both, similarity 0.1/0.9 and ingroup/outgroup, within subjects. And we do four groups of 5
        blocks. E.g., 5 blocks: similarity 0.1 & ingroup, then 5 blocks: similarity 0.1 & outgroup, then 5 blocks:
        similarity 0.9 & ingroup, then 5 blocks: similarity 0.9 & outgroup. We can calculate the number of possible permutations like so:
        P(n,r) = n!/(n-r!), where P="Permutations", n=number of treatment combinations=4, and r=number of block-groups=4. Thus, we have:
        P=4!/(4-4!)=24. There are 24 possible permutations of these 4x5 blocks. Below we assign every participant an integer that is an 
        element of the interval = [1,24]. Every integer stands for one of the 24 possible permutations and we counterbalance those across
        subjects. Specifically, we counterbalance them across social learners in the cognitive load treatment and across social learners in 
        the no-cognitive load treatment. Later we will assign players/participants treatment values of their treatment fields; 
        similar_ingroup and observing_ingroup based on their treatment integer (element in [1,24]) at the beginning of every round. 
        Every five rounds, new treatment values have to be assigned."""

        start_value_treatment_permutations = subsession.session.config['start_value_treatment_permutations']
        subsession.session.start_value_treatment_permutations = start_value_treatment_permutations

        numbers = range(start_value_treatment_permutations, 240)

        permutation_number = [number for number in numbers]

        """Assign permutation number to participants in the no-cognitive load treatment:"""

        treatment_assign = itertools.cycle(permutation_number)
        for player in cognitive_load_0_participants:
            participant = player.participant
            player.treatment_assignment = next(treatment_assign)
            participant.treatment_assignment = player.treatment_assignment
            print('participant.treatment_assignment of participants in the cognitive load treatment', participant.treatment_assignment)

        """Assign permutation number to participants in the cognitive load treatment:"""

        treatment_assign = itertools.cycle(permutation_number)
        for player in cognitive_load_1_participants:
            participant = player.participant
            player.treatment_assignment = next(treatment_assign)
            participant.treatment_assignment = player.treatment_assignment
            print('participant.treatment_assignment of participants in the cognitive load treatment', participant.treatment_assignment)

        ############################################################################################################
        #                                SESSION VARIABLE: TREATMENT PERMUTATIONS                                  #
        ############################################################################################################

        treatment_combinations = [(0.9, 1), (0.9, 0), (0.1, 1), (0.1, 0)]

        permutations_treatment_combinations_short = list(itertools.permutations(treatment_combinations))

        permutations_treatment_combinations = 10*permutations_treatment_combinations_short

        permut_treat_combinations = subsession.session.config['permut_treat_combinations']

        subsession.session.permut_treat_combinations = permut_treat_combinations

        subsession.session.permut_treat_combinations = permutations_treatment_combinations

        ############################################################################################################
        ############################################################################################################
        #                                           SET GROUP MATRIX                                               #
        ############################################################################################################
        ############################################################################################################
        """I have to do the following grouping only because 'Waitpages' in oTree only work based on such grouping.
        Note again, this 'grouping' is not the same as the groups in my experiment (triangle and square). It is just
        a coincidence that groups here in oTree refer to the "Types" (A and B) in my experiment rather than to the 
        "Groups"."""
        group_matrix = [type_a_participants, type_b_participants]
        subsession.set_group_matrix(group_matrix)
    else:
        subsession.group_like_round(1)

############################################################################################################
############################################################################################################
#                                            GROUP FUNCTIONS                                               #
############################################################################################################
############################################################################################################


############################################################################################################
#                              DETERMINE COLOR OF DRAWN marble: INSTRUCTIONS                                 #
############################################################################################################
def determine_color_drawn_marble_practice_block(group: Group):
    """Function to randomize urn composition and determine color of marble drawn for every player. This function is only used for the
    instructions part. The function for determining color of marbles and calculating payoffs is shown further below:"""
    """For the practice block of type A participants, we specify in the following which urn has which composition of
    marbles; Whether there are 3 red / 1 blue in the left / right urn and vice versa; Whether there are 1 red / 3 blue
    in the left / right urn. I do this with the 'numpy' package which I import below under the name 'np'. 
    IMPORTANT: ONLY USE random.seed() FOR TESTING PURPOSES AS IT WILL CAUSE THE RANDOM INT GENERATOR TO ALWAYS GENERATE THE SAME INT."""
    # random.seed(123)
    import numpy as np
    red_marbles_majority_practice_block = np.random.uniform(0, 1)
    print('If "red_marbles_majority_practice_block" is < 0.5, then the left urn contains 3 red / 1 blue marble. If '
          '"red_marbles_majority_practice_block" is >= 0.5, the right urn contains this set of 3 red / 1 blue marble.')
    print('random number from uniform dist "red_marbles_majority_practice_block" is = ', red_marbles_majority_practice_block)
    if red_marbles_majority_practice_block >= 0.5:
        print(
            'there is a majority of red marbles in the RIGHT urn because the value',
            red_marbles_majority_practice_block,
            'is >= 0.5',
        )
    else:
        print(
            'there is a majority of red marbles in the LEFT urn because the value',
            red_marbles_majority_practice_block,
            'is < 0.5',
        )
    """Specify the color of the marble that is drawn from the left and right urn. A random.seed(123) as specified above
     generates a value of > 0.5 and thus, the right urn will contain 3 red / 1 marble. In that case, the probability of 
     drawing a red marble in the right urn is 75%.  It's the opposite probability in the left urn; 25%. Accordingly, the probability 
     of drawing a blue marble in the right urn is 25% and  it's the opposite probability in the left urn; 75%."""
    for p in group.get_players():
        if red_marbles_majority_practice_block >= 0.5:
            marble_left_urn = np.random.uniform(0, 1)
            print('The probability of drawing "red" marble from the left urn is only 25%. Thus, only if the random number drawn from uniform'
                  'dist stored under variable "marble_left_urn" for the focal player is > 0.75, then the color is red, otherwise its blue')
            print(
                'value of "marble_left_urn" for participant with id',
                p.participant.id_in_session,
                'is:',
                marble_left_urn,
            )
            if marble_left_urn <= 0.75:
                p.participant.color_drawn_marble_left = 'blue'
                print(
                    'because the value of "marble_left_urn" shown above is <= 0.75, the color of the marble that would be drawn '
                    'from the left urn, if the focal participant chooses the left urn, is:',
                    p.participant.color_drawn_marble_left,
                )
            else:
                p.participant.color_drawn_marble_left = 'red'
                print(
                    'because the value of "marble_left_urn" shown above is > 0.75, the color of the marble that would be drawn '
                    'from the left urn, if the focal participant chooses the left urn, is:',
                    p.participant.color_drawn_marble_left,
                )
            marble_right_urn = np.random.uniform(0, 1)
            print('The probability of drawing a "red" marble from the right urn is 75%. Thus, if the random number drawn from uniform '
                  'dist stored under variable "marble_right_urn" for the focal player is <= 0.75, then the color is red, otherwise its blue')
            print(
                'value of "marble_right_urn" for participant with id',
                p.participant.id_in_session,
                'is:',
                marble_right_urn,
            )
            if marble_right_urn <= 0.75:
                p.participant.color_drawn_marble_right = 'red'
                print(
                    'because the value of "marble_right_urn" shown above is <= 0.75, the color of the marble that would be drawn '
                    'from the right urn, if the focal participant chooses the right urn, is:',
                    p.participant.color_drawn_marble_right,
                )
            else:
                p.participant.color_drawn_marble_right = 'blue'
                print(
                    'because the value of "marble_right_urn" shown above is > 0.75, the color of the marble that would be drawn '
                    'from the right urn, if the focal participant chooses the right urn, is:',
                    p.participant.color_drawn_marble_right,
                )
        else:
            marble_drawn_left_urn_when_left_urn_red_majority = np.random.uniform(0, 1)
            print('The probability of drawing a "red" marble from the left urn is 75%. Thus, if the random number drawn from uniform '
                  'dist stored under variable "marble_drawn_left_urn_when_left_urn_red_majority" for the focal player is <= 0.75, then the '
                  'color of the drawn marble is red, otherwise its blue')
            print(
                'value of "marble_drawn_left_urn_when_left_urn_red_majority" for participant with id',
                p.participant.id_in_session,
                'is:',
                marble_drawn_left_urn_when_left_urn_red_majority,
            )
            if marble_drawn_left_urn_when_left_urn_red_majority <= 0.75:
                p.participant.color_drawn_marble_left = 'red'
                print(
                    'because the value of "marble_drawn_left_urn_when_left_urn_red_majority" shown above is <= 0.75, the color of the marble '
                    'that would be drawn from the left urn, if the focal participant chooses the left urn, is:',
                    p.participant.color_drawn_marble_left,
                )
            else:
                p.participant.color_drawn_marble_left = 'blue'
                print(
                    'because the value of "marble_drawn_left_urn_when_left_urn_red_majority" shown above is > 0.75, the color of the marble '
                    'that would be drawn from the left urn, if the focal participant chooses the left urn, is:',
                    p.participant.color_drawn_marble_left,
                )
            marble_drawn_right_urn_when_left_urn_red_majority = np.random.uniform(0, 1)
            print('The probability of drawing a "red" marble from the right urn is only 25%. Thus, only if the random number drawn from '
                  'uniform dist stored under variable "marble_drawn_right_urn_when_left_urn_red_majority" for the focal player is > 0.75, '
                  'then the color is red, otherwise its blue')
            print(
                'value of "marble_drawn_right_urn_when_left_urn_red_majority" for participant with id',
                p.participant.id_in_session,
                'is:',
                marble_drawn_right_urn_when_left_urn_red_majority,
            )
            if marble_drawn_right_urn_when_left_urn_red_majority <= 0.75:
                p.participant.color_drawn_marble_right = 'blue'
                print(
                    'because the value of "marble_right_urn" shown above is <= 0.75, the color of the marble that would be drawn '
                    'from the right urn, if the focal participant chooses the right urn, is:',
                    p.participant.color_drawn_marble_right,
                )
            else:
                p.participant.color_drawn_marble_right = 'red'
                print(
                    'because the value of "marble_right_urn" shown above is > 0.75, the color of the marble that would be drawn '
                    'from the right urn, if the focal participant chooses the right urn, is:',
                    p.participant.color_drawn_marble_right,
                )


############################################################################################################
#           DETERMINE COLOR OF DRAWN marble AND CALCULATE PAYOFFS: EXPERIMENT FUNCTION                       #
############################################################################################################
"""Below function had to be written on group level because I needed access to players from different groups. I was not able to write this 
function on the session level. This is also the reason why I coded manually for every five rounds to access the position (left vs right urn) 
of red_marbles_majority (where red_marbles_majority=0 means there are 3 red and 1 blue marble in the left urn and vice versa)."""


def color_and_payoff_type_a(group):
    import numpy as np
    for p in group.get_players():
        round_number = p.round_number
        print('round number:', round_number)
        participant = p.participant
        print('participant:', participant)
        if round_number <= 4:
            red_marbles_majority = p.subsession.session.red_marbles_majority[0]
        elif 4 < round_number <= 8:
            red_marbles_majority = p.subsession.session.red_marbles_majority[1]
        elif 8 < round_number <= 12:
            red_marbles_majority = p.subsession.session.red_marbles_majority[2]
        elif 12 < round_number <= 16:
            red_marbles_majority = p.subsession.session.red_marbles_majority[3]
        elif 16 < round_number <= 20:
            red_marbles_majority = p.subsession.session.red_marbles_majority[4]
        elif 20 < round_number <= 24:
            red_marbles_majority = p.subsession.session.red_marbles_majority[5]
        elif 24 < round_number <= 28:
            red_marbles_majority = p.subsession.session.red_marbles_majority[6]
        elif 28 < round_number <= 32:
            red_marbles_majority = p.subsession.session.red_marbles_majority[7]
        elif 32 < round_number <= 36:
            red_marbles_majority = p.subsession.session.red_marbles_majority[8]
        elif 36 < round_number <= 40:
            red_marbles_majority = p.subsession.session.red_marbles_majority[9]
        elif 40 < round_number <= 44:
            red_marbles_majority = p.subsession.session.red_marbles_majority[10]
        elif 44 < round_number <= 48:
            red_marbles_majority = p.subsession.session.red_marbles_majority[11]
        elif 48 < round_number <= 52:
            red_marbles_majority = p.subsession.session.red_marbles_majority[12]
        elif 52 < round_number <= 56:
            red_marbles_majority = p.subsession.session.red_marbles_majority[13]
        elif 56 < round_number <= 60:
            red_marbles_majority = p.subsession.session.red_marbles_majority[14]
        elif 60 < round_number <= 64:
            red_marbles_majority = p.subsession.session.red_marbles_majority[15]
        elif 64 < round_number <= 68:
            red_marbles_majority = p.subsession.session.red_marbles_majority[16]
        elif 68 < round_number <= 72:
            red_marbles_majority = p.subsession.session.red_marbles_majority[17]
        elif 72 < round_number <= 76:
            red_marbles_majority = p.subsession.session.red_marbles_majority[18]
        elif 76 < round_number <= 80:
            red_marbles_majority = p.subsession.session.red_marbles_majority[19]
        print('red_marbles_majority in this round is;',
              red_marbles_majority,
              'and recall that if red_marbles_majority = 0 we have a majority of red marbles in the left urn and if '
              'red_marbles_majority = 1 we have a majority of red marbles in the right urn.')
        p.subsession.session.red_majority = red_marbles_majority
        p.subsession.red_majority = red_marbles_majority
        print('And every round, we store this value of red_marbles_majority on the session variable red_majority and to check '
              'whether it worked, we print p.subsession.session.red_majority here:'
              , p.subsession.session.red_majority)
        """Before calculating payoffs, we also determine this participants optimal urn based on the winning color and 
        the above determined value for red_marbles_majority:"""
        if p.participant.group_assignment == C.GROUP_TRIANGLE:
            if p.subsession.session.winning_color_triangle == 'red':
                if red_marbles_majority == 0:
                    p.optimal_urn = 0
                    p.participant.optimal_urn = p.optimal_urn
                elif red_marbles_majority == 1:
                    p.optimal_urn = 1
                    p.participant.optimal_urn = p.optimal_urn
            elif p.subsession.session.winning_color_triangle == 'blue':
                if red_marbles_majority == 0:
                    p.optimal_urn = 1
                    p.participant.optimal_urn = p.optimal_urn
                elif red_marbles_majority == 1:
                    p.optimal_urn = 0
                    p.participant.optimal_urn = p.optimal_urn
        elif p.participant.group_assignment == C.GROUP_SQUARE:
            if p.subsession.session.winning_color_square == 'red':
                if red_marbles_majority == 0:
                    p.optimal_urn = 0
                    p.participant.optimal_urn = p.optimal_urn
                elif red_marbles_majority == 1:
                    p.optimal_urn = 1
                    p.participant.optimal_urn = p.optimal_urn
            elif p.subsession.session.winning_color_square == 'blue':
                if red_marbles_majority == 0:
                    p.optimal_urn = 1
                    p.participant.optimal_urn = p.optimal_urn
                elif red_marbles_majority == 1:
                    p.optimal_urn = 0
                    p.participant.optimal_urn = p.optimal_urn
        """Now we determine the color of the drawn marble and calculate payoffs:"""
        probability_draw_red_marble = None
        """If there is a majority of red marbles in the left urn (red_marbles_majority=0) then the probability of drawing a red marble from 
        the left urn (participant.urn_choice = 0) is 0.75 and from the right urn it's 0.25. Vice versa for when the majority of red
        marbles is in the right urn (red_marbles_majority=1):"""
        if red_marbles_majority == 0:
            if participant.urn_choice == 0:
                probability_draw_red_marble = 0.75
                print('This player has chosen the LEFT urn and thus, the probability of drawing a red marble is 75% because there is a '
                      'majority of red marbles in the LEFT urn.')
            elif participant.urn_choice == 1:
                probability_draw_red_marble = 0.25
                print('This player has chosen the RIGHT urn and thus, the probability of drawing a red marble is 25% because there is a '
                      'majority of red marbles in the LEFT urn, and hence, a majority of blue marbles in the RIGHT urn.')
        elif red_marbles_majority == 1:
            if participant.urn_choice == 0:
                probability_draw_red_marble = 0.25
                print('This player has chosen the LEFT urn and thus, the probability of drawing a red marble is 25% because there is a '
                      'majority of red marbles in the RIGHT urn, and hence, a majority of blue marbles in the LEFT urn.')
            elif participant.urn_choice == 1:
                probability_draw_red_marble = 0.75
                print('This player has chosen the RIGHT urn and thus, the probability of drawing a red marble is 75% because there is a '
                      'majority of red marbles in the RIGHT urn.')
        """Draw random random from uniform distribution for the player and see whether it's <= the probability of drawing a red marble:"""
        marble_drawn = np.random.uniform(0, 1)
        print('value of marble_drawn is:', marble_drawn)
        if marble_drawn <= probability_draw_red_marble:
            p.color_drawn_marble = 'red'
            participant.color_drawn_marble = p.color_drawn_marble
            print(
                'Because value of "marble_drawn" shown above is <=', probability_draw_red_marble,
                'color of marble drawn from participant urn choice, which is', participant.urn_choice, 'and thus, is left if "False" and '
                'right if "True", is:', p.participant.color_drawn_marble,
            )
            """Calculate payoffs and store them under player variable. Player class has an automatic payoff field. And participant has an
            automatic payoff field, that will automatically sum over all player.payoffs, so I don't have to store payoff in a participant
            field additionally:"""
            if p.participant.group_assignment == C.GROUP_TRIANGLE:
                if p.participant.color_drawn_marble == p.subsession.session.winning_color_triangle:
                    p.payoff = p.payoff + 100
                else:
                    p.payoff = p.payoff + 0
            elif p.participant.group_assignment == C.GROUP_SQUARE:
                if p.participant.color_drawn_marble == p.subsession.session.winning_color_square:
                    p.payoff = p.payoff + 100
                else:
                    p.payoff = p.payoff + 0
        elif marble_drawn > probability_draw_red_marble:
            p.color_drawn_marble = 'blue'
            participant.color_drawn_marble = p.color_drawn_marble
            print(
                'Because value of "marble_drawn" shown above is >', probability_draw_red_marble,
                ', color of marble drawn from participant urn choice, which is', participant.urn_choice, 'and thus, is left if "False" and '
                'right if "True", is:', p.participant.color_drawn_marble,
            )
            if p.participant.group_assignment == C.GROUP_TRIANGLE:
                if p.participant.color_drawn_marble == p.subsession.session.winning_color_triangle:
                    p.payoff = p.payoff + 100
                else:
                    p.payoff = p.payoff + 0
            elif p.participant.group_assignment == C.GROUP_SQUARE:
                if p.participant.color_drawn_marble == p.subsession.session.winning_color_square:
                    p.payoff = p.payoff + 100
                else:
                    p.payoff = p.payoff + 0


############################################################################################################
#                         ASSIGN TREATMENT VALUES TO SOCIAL LEARNERS FOR INSTRUCTIONS                      #
############################################################################################################

"""This function does essentially the same as the function called "choice_dist_and_treat_assign" (except for the 
first part of the function "choice_dist_and_treat_assign" which I added later and which calculates the choice 
distributions of individual learners) further below. 
But instead of assigning treatment values every four rounds, it only assigns them in the first round. 
This is only needed for the instructions because during the instructions I already use several templates/pages
which refer to those treatment values of social learners. """


def assign_treatment_values_instructions(subsession: Subsession):
    if subsession.round_number == 1:
        print('the function assign_treatment_values_instructions is now being executed')
        print('subsession.round number:', subsession.round_number)
        for p in subsession.get_players():
            if p.participant.type_assignment == C.TYPE_B:
                participant = p.participant
                print('participant:', participant)
                print('participant.treatment_assignment:', p.participant.treatment_assignment)

                if subsession.round_number <= 20:
                    p.similar_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][0][0]
                    p.observing_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][0][1]
                    participant.similar_ingroup = p.similar_ingroup
                    print('participant.similar_ingroup:', participant.similar_ingroup)
                    participant.observing_ingroup = p.observing_ingroup
                    print('participant.observing_ingroup:', participant.observing_ingroup)
                elif 20 < subsession.round_number <= 40:
                    p.similar_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][1][0]
                    p.observing_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][1][1]
                    participant.similar_ingroup = p.similar_ingroup
                    print('participant.similar_ingroup:', participant.similar_ingroup)
                    participant.observing_ingroup = p.observing_ingroup
                    print('participant.observing_ingroup:', participant.observing_ingroup)
                elif 40 < subsession.round_number <= 60:
                    p.similar_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][2][0]
                    p.observing_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][2][1]
                    participant.similar_ingroup = p.similar_ingroup
                    print('participant.similar_ingroup:', participant.similar_ingroup)
                    participant.observing_ingroup = p.observing_ingroup
                    print('participant.observing_ingroup:', participant.observing_ingroup)
                elif 60 < subsession.round_number <= 80:
                    p.similar_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][3][0]
                    p.observing_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][3][1]
                    participant.similar_ingroup = p.similar_ingroup
                    print('participant.similar_ingroup:', participant.similar_ingroup)
                    participant.observing_ingroup = p.observing_ingroup
                    print('participant.observing_ingroup:', participant.observing_ingroup)
            else:
                pass


############################################################################################################
#  CALCULATE CHOICE DISTRIBUTIONS OF IND. LEARN. AND ASSIGN TREATMENT VALUES TO SOCIAL LEARNERS            #
############################################################################################################

"""Below function originally consisted of two separate functions (one for choice distribution calculation and
one for assigning treatments) but I wasn't able to execute them both simultaneously with the 
'after_all_players_arrive' function in the 'WaitForInstructionsStart(WaitPage)'. Thus, I integrated the 
calculation of choice distribution and the assignment of treatments in to one function.
Treatments will be assigned based on the permutations after groups of 5 blocks."""


def choice_dist_and_treat_assign(subsession: Subsession):
    """First we calculate choice distributions which will be used as social information for ind. learn.:"""
    if subsession.round_number >= 1:
        print('choice_distributions is now being executed')
        number_left_urn_triangle = 0
        print('number_left_urn_triangle', number_left_urn_triangle)
        number_left_urn_square = 0
        print('number_left_urn_square', number_left_urn_square)
        for p in subsession.get_players():
            participant = p.participant
            if p.participant.group_assignment == C.GROUP_TRIANGLE:
                if p.participant.type_assignment == C.TYPE_A:
                    print('type of focal player:', participant.type_assignment)
                    print('group of focal player:', participant.group_assignment)
                    if p.participant.urn_choice == 0:
                        number_left_urn_triangle = number_left_urn_triangle + 1
                        p.subsession.number_left_urn_triangle = number_left_urn_triangle
                    else:
                        number_left_urn_triangle = number_left_urn_triangle + 0
                        p.subsession.number_left_urn_triangle = number_left_urn_triangle
            elif p.participant.group_assignment == C.GROUP_SQUARE:
                if p.participant.type_assignment == C.TYPE_A:
                    print('type of focal player:', participant.type_assignment)
                    print('group of focal player:', participant.group_assignment)
                    if p.participant.urn_choice == 0:
                        number_left_urn_square = number_left_urn_square + 1
                        p.subsession.number_left_urn_square = number_left_urn_square
                    else:
                        number_left_urn_square = number_left_urn_square + 0
                        p.subsession.number_left_urn_square = number_left_urn_square
        print('subsession.number_left_urn_triangle', subsession.number_left_urn_triangle)
        print('subsession.number_left_urn_square', subsession.number_left_urn_square)

        """Below is the function for assigning the treatment values every fourth round:"""
        if subsession.round_number % 4 == 0:
            print('the function choice_dist_and_treat_assign is now being executed')
            print('subsession.round number:', subsession.round_number)
            for p in subsession.get_players():
                if p.participant.type_assignment == C.TYPE_B:
                    participant = p.participant
                    print('participant:', participant)
                    print('participant.treatment_assignment:', p.participant.treatment_assignment)
                    if subsession.round_number <= 20:
                        p.similar_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][0][0]
                        p.observing_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][0][1]
                        participant.similar_ingroup = p.similar_ingroup
                        print('participant.similar_ingroup:', participant.similar_ingroup)
                        participant.observing_ingroup = p.observing_ingroup
                        print('participant.observing_ingroup:', participant.observing_ingroup)
                    elif 20 < subsession.round_number <= 40:
                        p.similar_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][1][0]
                        p.observing_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][1][1]
                        participant.similar_ingroup = p.similar_ingroup
                        print('participant.similar_ingroup:', participant.similar_ingroup)
                        participant.observing_ingroup = p.observing_ingroup
                        print('participant.observing_ingroup:', participant.observing_ingroup)
                    elif 40 < subsession.round_number <= 60:
                        p.similar_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][2][0]
                        p.observing_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][2][1]
                        participant.similar_ingroup = p.similar_ingroup
                        print('participant.similar_ingroup:', participant.similar_ingroup)
                        participant.observing_ingroup = p.observing_ingroup
                        print('participant.observing_ingroup:', participant.observing_ingroup)
                    elif 60 < subsession.round_number <= 80:
                        p.similar_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][3][0]
                        p.observing_ingroup = p.session.permut_treat_combinations[p.participant.treatment_assignment][3][1]
                        participant.similar_ingroup = p.similar_ingroup
                        print('participant.similar_ingroup:', participant.similar_ingroup)
                        participant.observing_ingroup = p.observing_ingroup
                        print('participant.observing_ingroup:', participant.observing_ingroup)
                else:
                    pass


############################################################################################################
#           DETERMINE COLOR OF DRAWN marble AND CALCULATE PAYOFFS: EXPERIMENT FUNCTION                       #
############################################################################################################
"""Similarly to the version for type A players, below function had to be written on group level because I needed 
access to players from different groups. I was not able to write this function on the session level. 
Because the red_marbles_majority values (i.e. =0 if 3/4 red marbles are in the left urn in a given block and 
=1 if 3/4 red marbles are in the right urn) have been assigned to all players of both types (i.e. type A and B), I 
don't need to repeat that step in below function.
Some notes on the following calculation of payoffs: Social learners in the cognitive load treatment will receive 
payoffs for colors of drawn marbles normally as other players do but only in 10/20 blocks. To avoid income effects 
and to incentivize both memorizing task and urn choice task, in 10/20 blocks, social learners in the cognitive load
treatment will get paid for either 4 marbles randomly drawn from the optimal urn if they memorized the multi digit number 
correctly, or for 4 marbles randomly drawn from the suboptimal urn if they memorized the multi digit number 
incorrectly. """


def color_and_payoff_type_b(group):
    print('the payoff function for type B players is now being executed:')
    import numpy as np
    for p in group.get_players():
        round_number = p.round_number
        print('round number:', round_number)
        participant = p.participant
        print('participant:', participant)
        if p.participant.type_assignment == C.TYPE_B:
            print('Conduct the following only for type B players:')
            print('test whether I can access the session variable red_majority:',
                  p.subsession.red_majority)
            red_marbles_majority = p.subsession.red_majority
            print('test whether red_marbles_majority has been successfully assigned the value from p.subsession.red_majority:',
                  red_marbles_majority)
            """Now we determine the color of the drawn marble and calculate payoffs:"""
            probability_draw_red_marble = None
            """If there is a majority of red marbles in the left urn (red_marbles_majority=0) then the probability of drawing a red marble from 
            the left urn (participant.urn_choice = 0) is 0.75 and from the right urn it's 0.25. Vice versa for when the majority of red
            marbles is in the right urn (red_marbles_majority=1)."""
            if red_marbles_majority == 0:
                if participant.urn_choice == 0:
                    probability_draw_red_marble = 0.75
                    print('This social learner has chosen the LEFT urn and thus, the probability of drawing a red marble is '
                          '75% because there is a majority of red marbles in the LEFT urn.')
                elif participant.urn_choice == 1:
                    probability_draw_red_marble = 0.25
                    print('This social learner has chosen the RIGHT urn and thus, the probability of drawing a red marble is '
                          '25% because there is a majority of red marbles in the LEFT urn, and hence, a majority of blue marbles '
                          'in the RIGHT urn.')
            elif red_marbles_majority == 1:
                if participant.urn_choice == 0:
                    probability_draw_red_marble = 0.25
                    print('This social learner has chosen the LEFT urn and thus, the probability of drawing a red marble is '
                          '25% because there is a majority of red marbles in the RIGHT urn, and hence, a majority of blue marbles '
                          'in the LEFT urn.')
                elif participant.urn_choice == 1:
                    probability_draw_red_marble = 0.75
                    print('This social learner has chosen the RIGHT urn and thus, the probability of drawing a red marble is '
                          '75% because there is a majority of red marbles in the RIGHT urn.')
            """Evaluate whether the focal social learner receives points for the same color of points as their 
            ingroup or not. This simply means that they receive 100 points if that's the case and 0 otherwise. This 
            will be stored in a variable called payoff_increase which I will then use for every payoff calculation 
            in the following. The actual payoff will also depend on the color of the drawn marble and the winning color but
            this will be programmed further below."""
            similarity_prob = np.random.uniform(0, 1)
            """In each block, similar_ingroup (0.1/0.9) as a probability determines whether a focal social learner will
            receive points for the same or opposite color of marbles as their ingroup. For that we draw a random number each
            round. If the random number <= the value of similar_ingroup, they receive points for the same color of marbles
            as their ingroup and vice versa. """
            print('In this block, the random number drawn for the focal social player is:', similarity_prob)
            """In addition to registering the points, I will also register in a player field, whether the social learner
            shares the winning color with their ingroup in this specific block"""
            print('IN THIS BLOCK, THE SIMILARITY OF THIS PARTICIPANT IS:', p.participant.similar_ingroup)
            if p.participant.similar_ingroup == 0.9:
                if similarity_prob <= 0.9:
                    payoff_increase = 100
                    p.share_winning_color = 1
                    p.participant.share_winning_color = p.share_winning_color
                else:
                    payoff_increase = 0
                    p.share_winning_color = 0
                    p.participant.share_winning_color = p.share_winning_color
            elif p.participant.similar_ingroup == 0.1:
                if similarity_prob <= 0.1:
                    payoff_increase = 100
                    p.share_winning_color = 1
                    p.participant.share_winning_color = p.share_winning_color
                else:
                    payoff_increase = 0
                    p.share_winning_color = 0
                    p.participant.share_winning_color = p.share_winning_color
            print('payoff_increase for this focal player based on similarity is:', payoff_increase)
            """Before calculating payoffs, we also determine this participants optimal urn based on the winning color, 
            the further above determined value for red_marbles_majority and their value for the treatment 'similar_ingroup':
            Note, the following part of the code is currently not in the most efficient form right now because I quickly made it to 
            check whether choice data, payoffs, etc. of social learners match in terms of whether they chose the optimal urn or not.
            Some parts of the code could be done in a way that needs less lines of code, etc. Further, evaluating the optimal 
            urn is something that can be determined with the data already provided by the experiment and thus, this part of the code
            can be deleted if it takes too long to be executed -> need to test with a certain number of social learners."""
            if p.participant.group_assignment == C.GROUP_TRIANGLE:
                if p.subsession.session.winning_color_triangle == 'red':
                    if red_marbles_majority == 0:
                        if p.participant.similar_ingroup == 0.9:
                            if similarity_prob <= 0.9:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.9:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
                        elif p.participant.similar_ingroup == 0.1:
                            if similarity_prob <= 0.1:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.1:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
                    elif red_marbles_majority == 1:
                        if p.participant.similar_ingroup == 0.9:
                            if similarity_prob <= 0.9:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.9:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                        elif p.participant.similar_ingroup == 0.1:
                            if similarity_prob <= 0.1:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.1:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                elif p.subsession.session.winning_color_triangle == 'blue':
                    if red_marbles_majority == 0:
                        if p.participant.similar_ingroup == 0.9:
                            if similarity_prob <= 0.9:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.9:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                        elif p.participant.similar_ingroup == 0.1:
                            if similarity_prob <= 0.1:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.1:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                    elif red_marbles_majority == 1:
                        if p.participant.similar_ingroup == 0.9:
                            if similarity_prob <= 0.9:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.9:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
                        elif p.participant.similar_ingroup == 0.1:
                            if similarity_prob <= 0.1:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.1:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
            elif p.participant.group_assignment == C.GROUP_SQUARE:
                if p.subsession.session.winning_color_square == 'red':
                    if red_marbles_majority == 0:
                        if p.participant.similar_ingroup == 0.9:
                            if similarity_prob <= 0.9:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.9:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
                        elif p.participant.similar_ingroup == 0.1:
                            if similarity_prob <= 0.1:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.1:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
                    elif red_marbles_majority == 1:
                        if p.participant.similar_ingroup == 0.9:
                            if similarity_prob <= 0.9:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.9:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                        elif p.participant.similar_ingroup == 0.1:
                            if similarity_prob <= 0.1:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.1:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                elif p.subsession.session.winning_color_square == 'blue':
                    if red_marbles_majority == 0:
                        if p.participant.similar_ingroup == 0.9:
                            if similarity_prob <= 0.9:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.9:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                        elif p.participant.similar_ingroup == 0.1:
                            if similarity_prob <= 0.1:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.1:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                    elif red_marbles_majority == 1:
                        if p.participant.similar_ingroup == 0.9:
                            if similarity_prob <= 0.9:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.9:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
                        elif p.participant.similar_ingroup == 0.1:
                            if similarity_prob <= 0.1:
                                p.optimal_urn = 0
                                p.participant.optimal_urn = p.optimal_urn
                            elif similarity_prob > 0.1:
                                p.optimal_urn = 1
                                p.participant.optimal_urn = p.optimal_urn
            """NOTE: I mentioned that part of the code can be deleted if it takes too long. That concerns the code between the line
             where I mention this and this point here. The following part of the function is essential and should not be deleted.
            As described elsewhere, I have to do two different versions of payoff calculations for social learners in the 
            cognitive load treatment vs the ones who are not in the cognitive load treatment."""
            if p.participant.pay_task == 's':
                print('This is now the payoff function for the social learners who get points for the social learning task and to check'
                      'whether we can acess the number of rounds per block through the session.config variable, because we need this to '
                      'draw a number of marbles from the chosen urn, we print it here:', p.subsession.session.config['num_rounds_per_block'])
                for x in range(p.subsession.session.config['num_rounds_per_block']):
                    """HAVE TO DRAW 4 marbleS FROM CHOSEN URN -> use 'p.subsession.session.config['num_rounds_per_block']' in case I reuse 
                    this experiment with a different number of rounds I can change the number in settings.py:"""
                    """First determine the color of the drawn marble:"""
                    marble_drawn = np.random.uniform(0, 1)
                    print('value of marble_drawn is:', marble_drawn)
                    if marble_drawn <= probability_draw_red_marble:
                        p.color_drawn_marble = 'red'
                        participant.color_drawn_marble = p.color_drawn_marble
                        print(
                            'Because value of "marble_drawn" shown above is <=', probability_draw_red_marble,
                            'color of marble drawn from left urn is:', p.participant.color_drawn_marble,
                        )
                    elif marble_drawn > probability_draw_red_marble:
                        p.color_drawn_marble = 'blue'
                        participant.color_drawn_marble = p.color_drawn_marble
                        print('Because value of "marble_drawn" shown above is >', probability_draw_red_marble,
                              ', color of marble drawn from left urn is:', p.participant.color_drawn_marble)
                    """Calculate payoffs and store them under player variable. Player class has an automatic payoff field. And participant has an
                    automatic payoff field, that will automatically sum over all player.payoffs, so I don't have to store payoff in a participant
                    field additionally:"""
                    """I will include here also the aspect of the treatment 'similarity'. I.e., if a social learner has similarity
                    =0.9, then there is a probability of 90% that they receive points for the same color of marbles as their ingroup
                    and vice versa. Similarly for similarity = 0.1. To do that, I simply generate a random number from the uniform
                    distribution for each participant and then if the participant has similarity=0.9, they receive 100 points if 
                    the random number is <= 0.9 and 0 points otherwise. Similarly for participants with similarity=0.1. I do this
                    in each block instead of choosing 2/20 blocks for similarity=0.1 or 18/20 blocks for similarity=0.9 in which
                    social learners receive points for either same or opposite color of marbles as their ingroup. Doesn't matter 
                    which approach I choose in terms of incentivizing participants."""
                    if p.participant.group_assignment == C.GROUP_TRIANGLE:
                        if p.participant.color_drawn_marble == p.subsession.session.winning_color_triangle:
                            p.payoff = p.payoff + payoff_increase
                        else:
                            p.payoff = p.payoff + (100 - payoff_increase)
                    elif p.participant.group_assignment == C.GROUP_SQUARE:
                        if p.participant.color_drawn_marble == p.subsession.session.winning_color_square:
                            p.payoff = p.payoff + payoff_increase
                        else:
                            p.payoff = p.payoff + (100 - payoff_increase)
            elif p.participant.pay_task == 'm':
                print('The task that was selected for this focal participant to get points for is', p.participant.pay_task, ', where "m" '
                      'refers to the memorizing task.')
                if p.participant.memorized_correctly == 1:
                    print('This social learner has memorized the multi digit number correctly and thus,'
                          'payoff_probability is:')
                    """Here we simply use probability of 0.75 to give payoff=100 and do this for times"""
                    payoff_probability = 0.75
                    print(payoff_probability)
                    print('Now, 4 marbles are randomly drawn from the optimal urn. The optimal urn has a probability of 0.75'
                          'to get 100 points. Thus, if the following number called marble_drawn is <= 0.75, this social learner'
                          'will receive 100 points.')
                    for x in range(p.subsession.session.config['num_rounds_per_block']):
                        marble_drawn = np.random.uniform(0, 1)
                        print('The value of the randomly generated number called marble_drawn is:', marble_drawn)
                        if marble_drawn <= payoff_probability:
                            p.payoff = p.payoff + 100
                        else:
                            p.payoff = p.payoff + 0
                elif p.participant.memorized_correctly == 0:
                    print('This social learner has memorized the multi digit number incorrectly and thus,'
                          'payoff_probability is:')
                    """Here we simply use probability of 0.25 to give payoff=100 and do this for times"""
                    payoff_probability_low = 0.25
                    print('Now, 4 marbles are randomly drawn from the suboptimal urn. The suboptimal urn has a probability of 0.25'
                          'to get 100 points. Thus, if the following number called marble_drawn_suboptimal is <= 0.25, this '
                          'social learner will receive 100 points.')
                    for x in range(p.subsession.session.config['num_rounds_per_block']):
                        marble_drawn_suboptimal = np.random.uniform(0, 1)
                        if marble_drawn_suboptimal <= payoff_probability_low:
                            p.payoff = p.payoff + 100
                        else:
                            p.payoff = p.payoff + 0
        else:
            pass


############################################################################################################
#                                         CALCULATE CHOICE DISTRIBUTION                                    #
############################################################################################################
"""The following function has been integrated into the function choice_dist_and_treat_assign and is thus no
longer needed (but I keep it in case I need to execute it separately again)."""


def choice_distribution(subsession: Subsession):
    if subsession.round_number % 4 == 0:
        print('the function choice_distribution is now being executed')
        number_left_urn_triangle = 0
        print('number_left_urn_triangle', number_left_urn_triangle)
        number_left_urn_square = 0
        print('number_left_urn_square', number_left_urn_square)
        for p in subsession.get_players():
            participant = p.participant
            if p.participant.group_assignment == C.GROUP_TRIANGLE:
                if p.participant.type_assignment == C.TYPE_A:
                    print('type of focal player:', participant.type_assignment)
                    print('group of focal player:', participant.group_assignment)
                    if p.participant.urn_choice == 0:
                        number_left_urn_triangle = number_left_urn_triangle + 1
                        p.subsession.number_left_urn_triangle = number_left_urn_triangle
                    else:
                        number_left_urn_triangle = number_left_urn_triangle + 0
                        p.subsession.number_left_urn_triangle = number_left_urn_triangle
            elif p.participant.group_assignment == C.GROUP_SQUARE:
                if p.participant.type_assignment == C.TYPE_A:
                    print('type of focal player:', participant.type_assignment)
                    print('group of focal player:', participant.group_assignment)
                    if p.participant.urn_choice == 0:
                        number_left_urn_square = number_left_urn_square + 1
                        p.subsession.number_left_urn_square = number_left_urn_square
                    else:
                        number_left_urn_square = number_left_urn_square + 0
                        p.subsession.number_left_urn_square = number_left_urn_square
                        print('subsession.number_left_urn_triangle', subsession.number_left_urn_triangle)
                        print('subsession.number_left_urn_square', subsession.number_left_urn_square)

############################################################################################################
############################################################################################################
#                                            PLAYER FUNCTIONS                                              #
############################################################################################################
############################################################################################################


############################################################################################################
#                                              CUSTOM EXPORT                                               #
############################################################################################################
# From https://otree.readthedocs.io/en/latest/admin.html?highlight=custom_export#custom-data-exports:
"""
You can make your own custom data export for an app. In oTree Studio, go to the “Player” model and click on 
“custom_export” at the bottom. (If using a text editor, define the below function.) The argument players is a queryset 
of all the players in the database. Use a yield for each row of data.

def custom_export(players):
    # header row
    yield ['session', 'participant_code', 'round_number', 'id_in_group', 'payoff']
    for p in players:
        participant = p.participant
        session = p.session
        yield [session.code, participant.code, p.round_number, p.id_in_group, p.payoff]
        
Once this function is defined, your custom data export will be available in the regular data export page.
"""


def custom_export(players):
    # header row
    yield ['session.code', 'num_rounds_total', 'num_rounds_per_block', 'length_multi_digit', 'timeout_seconds_multi_digit',
           'winning_color_triangle', 'red_majority', 'number_left_urn_triangle', 'number_left_urn_square', 'round_number',
           'participant.code', 'id_in_group', 'wrong_answer_1', 'wrong_answer_2', 'wrong_answer_3', 'wrong_answer_4', 'wrong_answer_5',
           'wrong_answer_6', 'wrong_answer_7',
           'payoff', 'participant_payoff', 'consent', 'treatment_assignment', 'cognitive_load', 'similar_ingroup',
           'observing_ingroup', 'type_assignment', 'group_assignment', 'urn_choice', 'optimal_urn', 'color_drawn_marble',
           'memorized_correctly', 'memorize_number', 'recalled_number', 'share_winning_color', 'pay_task',
           'q1', 'q2', 'q3', 'q4nocl', 'q4', 'q5nocl', 'q5', 'q6', 'q7', 'q8', 'age', 'gender', 'study_subject', 'monetary_earnings']
    for p in players:
        participant = p.participant
        session = p.session
        subsession = p.subsession
        participant.consent = p.consent
        participant.cognitive_load = p.cognitive_load
        participant.treatment_assignment = p.treatment_assignment
        participant.similar_ingroup = p.similar_ingroup
        participant.observing_ingroup = p.observing_ingroup
        participant.type_assignment = p.type_assignment
        participant.group_assignment = p.group_assignment
        participant.urn_choice = p.urn_choice
        participant.optimal_urn = p.optimal_urn
        participant.color_drawn_marble = p.color_drawn_marble
        participant.memorized_correctly = p.memorized_correctly
        participant.memorize_number = p.memorize_number
        participant.recalled_number = p.recalled_number
        participant.share_winning_color = p.share_winning_color
        participant.pay_task = p.pay_task
        participant.q1 = p.q1
        participant.q2 = p.q2
        participant.q3 = p.q3
        participant.q4nocl = p.q4nocl
        participant.q4 = p.q4
        participant.q5nocl = p.q5nocl
        participant.q5 = p.q5
        participant.q6 = p.q6
        participant.q7 = p.q7
        participant.q8 = p.q8
        participant.age = p.age
        participant.gender = p.gender
        participant.study_subject = p.study_subject
        participant.monetary_earnings = p.monetary_earnings
        participant.wrong_answer_1 = p.wrong_answer_1
        participant.wrong_answer_2 = p.wrong_answer_2
        participant.wrong_answer_3 = p.wrong_answer_3
        participant.wrong_answer_4 = p.wrong_answer_4
        participant.wrong_answer_5 = p.wrong_answer_5
        participant.wrong_answer_6 = p.wrong_answer_6
        participant.wrong_answer_7 = p.wrong_answer_7

        yield [session.code, session.config['num_rounds_total'], session.config['num_rounds_per_block'],
               session.config['length_multi_digit'], session.config['timeout_seconds_multi_digit'],
               session.config['winning_color_triangle'],
               subsession.field_maybe_none('red_majority'), subsession.field_maybe_none('number_left_urn_triangle'),
               subsession.field_maybe_none('number_left_urn_square'),
               p.round_number, participant.code, p.id_in_group,
               p.wrong_answer_1, p.wrong_answer_2, p.wrong_answer_3, p.wrong_answer_4, p.wrong_answer_5, p.wrong_answer_6,
               p.wrong_answer_7,
               p.payoff,
               participant.payoff, participant.consent, participant.treatment_assignment,
               participant.cognitive_load, participant.similar_ingroup,
               participant.observing_ingroup, participant.type_assignment, participant.group_assignment, participant.urn_choice,
               participant.optimal_urn, participant.color_drawn_marble, participant.memorized_correctly,
               participant.memorize_number, participant.recalled_number, participant.share_winning_color, participant.pay_task,
               participant.q1, participant.q2, participant.q3, participant.q4nocl, participant.q4, participant.q5nocl, participant.q5, participant.q6, participant.q7,
               participant.q8, participant.age, participant.gender, participant.study_subject, participant.monetary_earnings]


########################################################################################################
#                                 DYNAMIC URN CHOICE BUTTON                                            #
########################################################################################################

"""From https://otree.readthedocs.io/en/latest/forms.html: If you want choices to be determined dynamically 
(e.g. different from player to player), then you can instead define one of the below functions.

{field_name}_choices()

Like setting choices=, this will set the choices for the form field (e.g. the dropdown menu or radio buttons).

Example:

class Player(BasePlayer):
    fruit = models.StringField()

def fruit_choices(player):
    import random
    choices = ['apple', 'kiwi', 'mango']
    random.shuffle(choices)
    return choices
"""


def urn_choice_choices(player: Player):
    choices = [
        [0, 'Left Urn'],
        [1, 'Right Urn'],
    ]
    random.shuffle(choices)
    return choices


########################################################################################################
#                                         QUIZ ERROR MESSAGES                                          #
########################################################################################################
def question1_error_message(player: Player, question1):
    print('The focal player has answered the following to question 1:', question1)
    if question1 != C.QUIZ_ANSWER_1:
        player.wrong_answer_1 = 1
        player.participant.wrong_answer_1 = 1
        return (
            "WRONG: The two urns contain always opposite compositions of marbles. One urn always contains 3 red "
            "marbles and 1 blue marble. The other urn contains 1 red marble and 3 blue marbles. Please try to answer "
            "the question again."
        )


def question2_error_message(player: Player, question2):
    print('The focal player has answered the following to question 1:', question2)
    if question2 != C.QUIZ_ANSWER_2:
        player.wrong_answer_2 = 1
        player.participant.wrong_answer_2 = 1
        return (
            "WRONG: The two urns contain always opposite compositions of marbles. One urn always contains 3 red "
            "marbles and 1 blue marble. The other urn contains 1 red marble and 3 blue marbles. Please try to answer "
            "the question again."
        )


# Question 3
def question3_error_message(player: Player, question3):
    print('The focal player has answered the following to question 3:', question3)
    if question3 != C.QUIZ_ANSWER_3:
        player.wrong_answer_3 = 1
        player.participant.wrong_answer_3 = 1
        return (
            "WRONG: Your group membership and your type have been determined at the beginning of the "
            "instructions and will not change for the rest of the study. Please try to answer "
            "the question again."
        )


# Question 4 Type A
def question4_type_a_error_message(player: Player, question4_type_a):
    print('The focal player has answered the following to question 4 for type a:', question4_type_a)
    if question4_type_a != C.QUIZ_TYPE_A_ANSWER_4:
        player.wrong_answer_4 = 1
        player.participant.wrong_answer_4 = 1
        return (
            "WRONG: One block lasts 4 rounds. In those 4 rounds, the urns are not shuffled and thus, you "
            "can try to find out which of both urns contains more marbles of your winning colour during those 4 rounds in a given block."
        )


# Question 4 Type B
def question4_type_b_error_message(player: Player, question4_type_b):
    print('The focal player has answered the following to question 4 for type B:', question4_type_b)
    if question4_type_b != C.QUIZ_TYPE_B_ANSWER_4:
        player.wrong_answer_4 = 1
        player.participant.wrong_answer_4 = 1
        if player.participant.group_assignment == C.GROUP_TRIANGLE:
            return (
                "Wrong: There is a certain probability with which you get points for the same color as "
                "your own group, the triangle group. In particular, in certain blocks you get points for"
                "the same colour of marbles as your own group with a probability of 10% and in other blocks "
                "with a probability of 90%. Thus, in a given block you don’t know for sure whether you earn "
                "points for the same color as type A participants of your own group, the triangle group, or "
                "the same color as type A participants of the other group, the square group. Please try to "
                "answer the question again."
            )
        elif player.participant.group_assignment == C.GROUP_SQUARE:
            return (
                "Wrong: There is a certain probability with which you get points for the same color as "
                "your own group, the square group. In particular, in certain blocks you get points for"
                "the same colour of marbles as your own group with a probability of 10% and in other blocks "
                "with a probability of 90%. Thus, in a given block you don’t know for sure whether you earn "
                "points for the same color as type A participants of your own group, the square group, or "
                "the same color as type A participants of the other group, the triangle group. Please try to "
                "answer the question again."
            )


# Question 5 Type A
def question5_type_a_error_message(player: Player, question5_type_a):
    print('The focal player has answered the following to question 5 for type a:', question5_type_a)
    if question5_type_a != C.QUIZ_TYPE_A_ANSWER_5:
        player.wrong_answer_5 = 1
        player.participant.wrong_answer_5 = 1
        return (
            "WRONG: One urn always contains 3 red marbles and 1 blue marble. The other urn contains 1 red marble "
            "and 3 blue marbles. Drawing a marble of a color of which there are 3 out of 4 in a given urn, happens with "
            "a probability of 75%. And drawing a marble of a color of which there is 1 out of 4 in a given urn, "
            "happens with a probability of 25%. Please try to answer the question again."
        )


# Question 5 Type B
def question5_type_b_error_message(player: Player, question5_type_b):
    print('The focal player has answered the following to question 5 for type B:', question5_type_b)
    if question5_type_b != C.QUIZ_TYPE_B_ANSWER_5:
        player.wrong_answer_5 = 1
        player.participant.wrong_answer_5 = 1
        return (
            "WRONG: Type A participants of the two groups, triangle and square, receive points for opposite "
            "colors. This also implies that, if in one given block the left urn is better for triangle type A "
            "participants because it has more marbles of their winning color, then the right urn must be better "
            "for square type A participants in this same block. Please try to answer the question again."
        )


# Question 6 Type A
def question6_type_a_error_message(player: Player, question6_type_a):
    print('The focal player has answered the following to question 6 for type a:', question6_type_a)
    if question6_type_a != C.QUIZ_TYPE_A_ANSWER_6:
        player.wrong_answer_6 = 1
        player.participant.wrong_answer_6 = 1
        return (
            "WRONG: One urn always contains 3 red marbles and 1 blue marble. The other urn contains 1 red marble "
            "and 3 blue marbles. Drawing a marble of a color of which there are 3 out of 4 in a given urn, happens with "
            "a probability of 75%. And drawing a marble of a color of which there is 1 out of 4 in a given urn, "
            "happens with a probability of 25%. Please try to answer the question again."
        )


# Question 6 Type B
def question6_type_b_error_message(player: Player, question6_type_b):
    print('The focal player has answered the following to question 6 for type B:', question6_type_b)
    if question6_type_b != C.QUIZ_TYPE_B_ANSWER_6:
        player.wrong_answer_6 = 1
        player.participant.wrong_answer_6 = 1
        return (
            "WRONG: At the beginning of the experiment it will be randomly determined with equal chance "
            "whether you will earn points for either the memorizing task or the urn choice task. We will not inform you which task was "
            "selected for you. If you only focus on one task and it was determined that you get points for the other task, "
            "it might be that you earn 0 points. So, it is best for you in terms of earning points if you focus on both tasks equally."
        )


# Question 7 Type B
def question7_type_b_error_message(player: Player, question7_type_b):
    print('The focal player has answered the following to question 7 for type B:', question7_type_b)
    if question7_type_b != C.QUIZ_TYPE_B_ANSWER_7:
        player.wrong_answer_7 = 1
        player.participant.wrong_answer_7 = 1
        return (
            "WRONG: Type A participants of the two groups, triangle and square, receive points for opposite "
            "colors. This also implies that, if in one given block the left urn is better for triangle type A "
            "participants because it has more marbles of their winning color, then the right urn must be better "
            "for square type A participants in this same block. Please try to answer the question again."
        )


#############################################################################################################
############################################################################################################
############################################################################################################
#                                                PAGES                                                     #
############################################################################################################
############################################################################################################
############################################################################################################


############################################################################################################
#                                             PURPLE DOTS                                                  #
############################################################################################################

class Dots(Page):
    form_model = 'player'
    
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        key = "abcdefg"
        host = player.participant.label
        return dict(
            key=key,
            host=host,
        )


############################################################################################################
#                                             INSTRUCTIONS                                                 #
############################################################################################################


class Consent(Page):
    """This page contains introductory info and asks for consent"""

    form_model = 'player'
    form_fields = ['consent']
    """Specify to only show page in the first round:"""

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    @staticmethod
    def vars_for_template(player):
        money = 100 * player.session.real_world_currency_per_point
        max_earning = 8000 * player.session.real_world_currency_per_point
        total_possible_earning = 10 + 8000 * player.session.real_world_currency_per_point

        return dict(
            money=money,
            max_earning=max_earning,
            total_possible_earning=total_possible_earning,
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        participant.consent = player.consent


class ConsentDisagree(Page):
    """Display this page only if participant disagrees with the consent terms."""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.consent == 0 and player.round_number == 1


class ConsentAgree(Page):
    """Display this page only if participant agrees with the consent terms."""
    form_model = 'player'

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.consent == 1 and player.round_number == 1

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        player.consent = participant.consent


class WaitForInstructionsStart(WaitPage):
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    wait_for_all_groups = True

    form_model = 'player'
    title_text = "Please wait"
    body_text = (
        "Please wait, until all participants have given consent to start the experiment."
    )

    after_all_players_arrive = 'assign_treatment_values_instructions'

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        participant.similar_ingroup = player.similar_ingroup
        participant.observing_ingroup = player.observing_ingroup


class Instructions1(Page):
    form_model = 'player'
    """Specify to only show page in the first round:"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    """Show group assignment animation"""
    @staticmethod
    def vars_for_template(player: Player):
        return dict(image_path='cognitive_load_exp/animations/group_assignmentnew.mp4')


class Instructions2(Page):
    form_model = 'player'
    """Specify to only show page in the first round:"""

    timeout_seconds = 30

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1


class Instructions3(Page):
    form_model = 'player'
    """Specify to only show page in the first round:"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    """Show type assignment animation"""
    @staticmethod
    def vars_for_template(player: Player):
        return dict(image_path='cognitive_load_exp/general_instructions/type_assignment.png')


class Instructions4(Page):
    form_model = 'player'
    """Specify to only show page in the first round:"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1


class Instructions5(Page):
    form_model = 'player'
    """Specify to only show page in the first round:"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1


class Instructions6(Page):
    form_model = 'player'
    """Specify to only show page in the first round:"""

    timeout_seconds = 30

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            image_path='cognitive_load_exp/type_group_assign/type_a_group_triangle.png',
            image_path2='cognitive_load_exp/type_group_assign/type_b_group_triangle.png',
            image_path3='cognitive_load_exp/type_group_assign/type_a_group_square.png',
            image_path4='cognitive_load_exp/type_group_assign/type_b_group_square.png',
        )


class Instructions7(Page):
    form_model = 'player'
    """Specify to only show page in the first round:"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            image_path='cognitive_load_exp/general_instructions/marbles_composition.png',
        )


class Instructions8(Page):
    form_model = 'player'
    """Specify to only show page in the first round:"""

    timeout_seconds = 60

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(image_path='cognitive_load_exp/animations/general_task_animation.mp4')


class Instructions9TypeA(Page):
    form_model = 'player'
    """Only show page to Type B"""

    timeout_seconds = 30

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1


class Instructions9TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1


class Instructions10TypeA(Page):
    form_model = 'player'
    """Only show page to Type A Triangle"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1


class Instructions10TypeBFirst(Page):
    form_model = 'player'
    """Only show page to Type B"""

    timeout_seconds = 30

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(same_urns_left_red_majority='cognitive_load_exp/general_instructions/same_urns_left_red_majority.png',
                    same_urns_right_red_majority='cognitive_load_exp/general_instructions/same_urns_right_red_majority.png')


class Instructions10TypeBSecond(Page):
    form_model = 'player'
    """Only show page to Type B"""

    timeout_seconds = 30

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(same_urns_left_red_majority='cognitive_load_exp/general_instructions/same_urns_left_red_majority.png',
                    same_urns_right_red_majority='cognitive_load_exp/general_instructions/same_urns_right_red_majority.png')


class Instructions11TypeA(Page):
    form_model = 'player'
    """Only show page to Type A Triangle"""

    timeout_seconds = 20

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    """In the following I make use of the parameter 'winning_color_triangle' that I stored in settings.py under
    SESSION_CONFIGS. The variable 'winning_color_triangle' is something we might want to vary across sessions and we
    can edit this in SESSION_CONFIGS. The variable 'winning_color_triangle' was then stored under
    'def creating_session(subsession: Subsession):' on the session level and thus, we can always access the variable 
    value by typing 'session.winning_color_triangle' or on templates with either
    {{ session.config.winning_color_triangle }} or; {{ session.vars.winning_color_triangle }}"""

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            image_path='cognitive_load_exp/winning_color/winning_color_'
            + player.session.winning_color_triangle
            + '.png',
            image_path2='cognitive_load_exp/winning_color/winning_color_'
            + player.session.winning_color_square
            + '.png',
        )


class Instructions11TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    timeout_seconds = 20

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(image_path='cognitive_load_exp/general_instructions/same_urns_greyed_out.png')


class Instructions12TypeA(Page):
    form_model = 'player'
    """Only show page to Type A"""

    timeout_seconds = 30

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1


class Instructions12TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    timeout_seconds = 30

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1


class Instructions13(Page):
    form_model = 'player'
    """Specify to only show page in the first round:"""

    timeout_seconds = 30

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(image_path='cognitive_load_exp/type_b_instructions/block_sequence.jpg')


class Instructions14(Page):
    form_model = 'player'
    """Specify to only show page in the first round:"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1


class Instructions15(Page):
    form_model = 'player'
    """Specify to only show page in the first round:"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1


class Instructions16TypeA(Page):
    form_model = 'player'
    """Only show page to Type A"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1


class Instructions16TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1


class Instructions17TypeA(Page):
    form_model = 'player'
    """Only show page to Type A"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1


class Instructions17TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1


class Instructions18TypeA(Page):
    form_model = 'player'
    """Only show page to Type A"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1


class Instructions19TypeA(Page):
    form_model = 'player'
    """Only show page to Type A"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        participant.urn_choice = player.field_maybe_none('urn_choice')


class Instructions20TypeA(Page):
    form_model = 'player'
    form_fields = ['urn_choice']
    """Only show page to Type A"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            image_path='cognitive_load_exp/example_block_type_a/urn_choice.png',
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        participant.urn_choice = player.urn_choice


class DetermineColorDrawnmarbleTypeAPracticeBlock(WaitPage):
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    form_model = 'player'
    title_text = "Please wait"
    body_text = (
        "We will soon show you the color of the drawn marble but we first wait for every type A participant "
        "to choose an urn."
    )
    after_all_players_arrive = 'determine_color_drawn_marble_practice_block'


class Instructions21TypeA(Page):
    form_model = 'player'
    """Only show page to Type A"""

    timeout_seconds = 15

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            wc_red_left_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_left_red_100.png',
            wc_red_left_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_left_blue_0.png',
            wc_red_right_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_right_red_100.png',
            wc_red_right_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_right_blue_0.png',
            wc_blue_left_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_left_red_0.png',
            wc_blue_left_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_left_blue_100.png',
            wc_blue_right_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_right_red_0.png',
            wc_blue_right_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_right_blue_100.png',
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        """Remove urn choice from this round because otherwise it will be saved as default on next round.:"""
        player.urn_choice = None
        participant.urn_choice = player.field_maybe_none('urn_choice')


class Instructions20TypeARound2(Page):
    form_model = 'player'
    form_fields = ['urn_choice']
    """Only show page to Type A"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            image_path='cognitive_load_exp/example_block_type_a/urn_choice.png',
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        participant.urn_choice = player.urn_choice


class DetermineColorDrawnmarbleTypeAPracticeBlockRound2(WaitPage):
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    form_model = 'player'
    title_text = "Please wait"
    body_text = (
        "We will soon show you the color of the drawn marble but we first wait for every type A participant "
        "to choose an urn."
    )
    after_all_players_arrive = 'determine_color_drawn_marble_practice_block'


class Instructions21TypeARound2(Page):
    form_model = 'player'
    """Only show page to Type A"""

    timeout_seconds = 15

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            wc_red_left_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_left_red_100.png',
            wc_red_left_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_left_blue_0.png',
            wc_red_right_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_right_red_100.png',
            wc_red_right_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_right_blue_0.png',
            wc_blue_left_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_left_red_0.png',
            wc_blue_left_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_left_blue_100.png',
            wc_blue_right_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_right_red_0.png',
            wc_blue_right_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_right_blue_100.png',
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        """Remove urn choice from this round because otherwise it will be saved as default on next round.:"""
        player.urn_choice = None
        participant.urn_choice = player.field_maybe_none('urn_choice')


class Instructions20TypeARound3(Page):
    form_model = 'player'
    form_fields = ['urn_choice']
    """Only show page to Type A"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            image_path='cognitive_load_exp/example_block_type_a/urn_choice.png',
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        participant.urn_choice = player.urn_choice


class DetermineColorDrawnmarbleTypeAPracticeBlockRound3(WaitPage):
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    form_model = 'player'
    title_text = "Please wait"
    body_text = (
        "We will soon show you the color of the drawn marble but we first wait for every type A participant "
        "to choose an urn."
    )
    after_all_players_arrive = 'determine_color_drawn_marble_practice_block'


class Instructions21TypeARound3(Page):
    form_model = 'player'
    """Only show page to Type A"""

    timeout_seconds = 15

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            wc_red_left_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_left_red_100.png',
            wc_red_left_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_left_blue_0.png',
            wc_red_right_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_right_red_100.png',
            wc_red_right_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_right_blue_0.png',
            wc_blue_left_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_left_red_0.png',
            wc_blue_left_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_left_blue_100.png',
            wc_blue_right_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_right_red_0.png',
            wc_blue_right_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_right_blue_100.png',
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        """Remove urn choice from this round because otherwise it will be saved as default on next round.:"""
        player.urn_choice = None
        participant.urn_choice = player.field_maybe_none('urn_choice')


class Instructions20TypeARound4(Page):
    form_model = 'player'
    form_fields = ['urn_choice']
    """Only show page to Type A"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            image_path='cognitive_load_exp/example_block_type_a/urn_choice.png',
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        participant.urn_choice = player.urn_choice


class DetermineColorDrawnmarbleTypeAPracticeBlockRound4(WaitPage):
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    form_model = 'player'
    title_text = "Please wait"
    body_text = (
        "We will soon show you the color of the drawn marble but we first wait for every type A participant "
        "to choose an urn."
    )
    after_all_players_arrive = 'determine_color_drawn_marble_practice_block'


class Instructions21TypeARound4(Page):
    form_model = 'player'
    """Only show page to Type A"""

    timeout_seconds = 15

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            wc_red_left_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_left_red_100.png',
            wc_red_left_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_left_blue_0.png',
            wc_red_right_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_right_red_100.png',
            wc_red_right_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_right_blue_0.png',
            wc_blue_left_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_left_red_0.png',
            wc_blue_left_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_left_blue_100.png',
            wc_blue_right_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_right_red_0.png',
            wc_blue_right_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_right_blue_100.png',
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        """Remove urn choice from this round because otherwise it will be saved as default on next round.:"""
        player.urn_choice = None
        participant.urn_choice = player.field_maybe_none('urn_choice')


class Instructions18TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1


class Instructions18TypeB2(Page):
    form_model = 'player'

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    """Below I define a variable, namely the multi-digit number, for the template and I save it in a player field with variable name 
    'memorize_number'), so that I can later refer to this number and check whether it's correct when players enter the number. 
    Note: I don't store it under participant variable, because participant variables would be constant across subsessions:"""

    @staticmethod
    def vars_for_template(player: Player):
        import random
        participant = player.participant
        length_multi_digit = player.session.length_multi_digit
        print('length_multi_digit:', length_multi_digit)
        multi_digit_number = [None] * length_multi_digit
        for i in range(0, length_multi_digit):
            multi_digit_number[i] = random.randrange(1, 9, 1)
        """First define a variable that shows the four list elements as separate characters/string-elements/letters 
        with spaces between the separate letters:"""
        show_multi_digit_number = ' '.join([str(elem) for elem in multi_digit_number])
        """Then do same but without spaces so that we can then compare this number to the number the subject will insert
        on template of the page 'Instructions32TypeB':"""
        player.memorize_number = ' '.join([str(elem) for elem in multi_digit_number])
        participant.memorize_number = player.memorize_number
        print('player.memorize_number', player.memorize_number)
        print('participant.memorize_number', participant.memorize_number)
        """Take variable above with spaces (show_multi_digit_number) and return in dictionary:"""
        return dict(show_multi_digit_number=show_multi_digit_number)


class Instructions18TypeB3(Page):
    form_model = 'player'

    def get_timeout_seconds(player):
        session = player.session
        return session.config['timeout_seconds_multi_digit']

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    """Below I define a variable, namely the multi-digit number, for the template and I save it in a player field with variable name 
    'memorize_number'), so that I can later refer to this number and check whether it's correct when players enter the number. 
    Note: I don't store it under participant variable, because participant variables would be constant across subsessions:"""

    @staticmethod
    def vars_for_template(player: Player):
        import random
        participant = player.participant
        length_multi_digit = player.session.length_multi_digit
        print('length_multi_digit:', length_multi_digit)
        multi_digit_number = [None] * length_multi_digit
        for i in range(0, length_multi_digit):
            multi_digit_number[i] = random.randrange(1, 9, 1)
        """First define a variable that shows the four list elements as separate characters/string-elements/letters 
        with spaces between the separate letters:"""
        show_multi_digit_number = ' '.join([str(elem) for elem in multi_digit_number])
        """Then do same but without spaces so that we can then compare this number to the number the subject will insert
        on template of the page 'Instructions32TypeB':"""
        player.memorize_number = ' '.join([str(elem) for elem in multi_digit_number])
        participant.memorize_number = player.memorize_number
        print('player.memorize_number', player.memorize_number)
        print('participant.memorize_number', participant.memorize_number)
        """Take variable above with spaces (show_multi_digit_number) and return in dictionary:"""
        return dict(show_multi_digit_number=show_multi_digit_number)


class Instructions19TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            image_path_triangle_01='cognitive_load_exp/similarity/triangle_01.png',
            image_path_triangle_09='cognitive_load_exp/similarity/triangle_09.png',
            image_path_square_01='cognitive_load_exp/similarity/square_01.png',
            image_path_square_09='cognitive_load_exp/similarity/square_09.png',
        )


class Instructions19TypeBSocInfo1(Page):
    form_model = 'player'
    """Only show page to Type B"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            square_in_01='cognitive_load_exp/example_blocks_type_b/square/square_in_01.png',
            square_in_09='cognitive_load_exp/example_blocks_type_b/square/square_in_09.png',
            square_out_01='cognitive_load_exp/example_blocks_type_b/square/square_out_01.png',
            square_out_09='cognitive_load_exp/example_blocks_type_b/square/square_out_09.png',
            triangle_in_01='cognitive_load_exp/example_blocks_type_b/triangle/triangle_in_01.png',
            triangle_in_09='cognitive_load_exp/example_blocks_type_b/triangle/triangle_in_09.png',
            triangle_out_01='cognitive_load_exp/example_blocks_type_b/triangle/triangle_out_01.png',
            triangle_out_09='cognitive_load_exp/example_blocks_type_b/triangle/triangle_out_09.png',
        )


class Instructions19TypeBSocInfo2(Page):
    form_model = 'player'
    """Only show page to Type B"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            square_in_01='cognitive_load_exp/example_blocks_type_b/square/square_in_01.png',
            square_in_09='cognitive_load_exp/example_blocks_type_b/square/square_in_09.png',
            square_out_01='cognitive_load_exp/example_blocks_type_b/square/square_out_01.png',
            square_out_09='cognitive_load_exp/example_blocks_type_b/square/square_out_09.png',
            triangle_in_01='cognitive_load_exp/example_blocks_type_b/triangle/triangle_in_01.png',
            triangle_in_09='cognitive_load_exp/example_blocks_type_b/triangle/triangle_in_09.png',
            triangle_out_01='cognitive_load_exp/example_blocks_type_b/triangle/triangle_out_01.png',
            triangle_out_09='cognitive_load_exp/example_blocks_type_b/triangle/triangle_out_09.png',
        )


class Instructions19TypeBSocInfo3(Page):
    form_model = 'player'
    """Only show page to Type B"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            square_in_01='cognitive_load_exp/example_blocks_type_b/square/square_in_01.png',
            square_in_09='cognitive_load_exp/example_blocks_type_b/square/square_in_09.png',
            square_out_01='cognitive_load_exp/example_blocks_type_b/square/square_out_01.png',
            square_out_09='cognitive_load_exp/example_blocks_type_b/square/square_out_09.png',
            triangle_in_01='cognitive_load_exp/example_blocks_type_b/triangle/triangle_in_01.png',
            triangle_in_09='cognitive_load_exp/example_blocks_type_b/triangle/triangle_in_09.png',
            triangle_out_01='cognitive_load_exp/example_blocks_type_b/triangle/triangle_out_01.png',
            triangle_out_09='cognitive_load_exp/example_blocks_type_b/triangle/triangle_out_09.png',
        )


class Instructions19TypeBSocInfo4(Page):
    form_model = 'player'
    """Only show page to Type B"""

    timeout_seconds = 40

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            square_in_01='cognitive_load_exp/example_blocks_type_b/square/square_in_01.png',
            square_in_09='cognitive_load_exp/example_blocks_type_b/square/square_in_09.png',
            square_out_01='cognitive_load_exp/example_blocks_type_b/square/square_out_01.png',
            square_out_09='cognitive_load_exp/example_blocks_type_b/square/square_out_09.png',
            triangle_in_01='cognitive_load_exp/example_blocks_type_b/triangle/triangle_in_01.png',
            triangle_in_09='cognitive_load_exp/example_blocks_type_b/triangle/triangle_in_09.png',
            triangle_out_01='cognitive_load_exp/example_blocks_type_b/triangle/triangle_out_01.png',
            triangle_out_09='cognitive_load_exp/example_blocks_type_b/triangle/triangle_out_09.png',
        )


class Instructions20TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1


class Instructions21TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1


class Instructions21TypeB2(Page):
    form_model = 'player'
    """Only show page to Type B"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1


class Instructions22TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1


class Instructions23TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1


class Instructions24TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1


class Instructions25TypeB(Page):
    form_model = 'player'

    def get_timeout_seconds(player):
        session = player.session

        return session.config['timeout_seconds_multi_digit']

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        import random
        participant = player.participant
        length_multi_digit = player.session.length_multi_digit
        print('length_multi_digit:', length_multi_digit)
        multi_digit_number = [None] * length_multi_digit
        for i in range(0, length_multi_digit):
            multi_digit_number[i] = random.randrange(1, 9, 1)
        """First define a variable that shows the four list elements as separate characters/string-elements/letters 
        with spaces between the separate letters:"""
        show_multi_digit_number = ' '.join([str(elem) for elem in multi_digit_number])
        """Then do same but without spaces so that we can then compare this number to the number the subject will insert
        on template of the page 'Instructions32TypeB':"""
        player.memorize_number = ''.join([str(elem) for elem in multi_digit_number])
        participant.memorize_number = player.memorize_number
        print('player.memorize_number', player.memorize_number)
        print('participant.memorize_number', participant.memorize_number)
        """Take variable above with spaces (show_multi_digit_number) and return in dictionary:"""
        return dict(show_multi_digit_number=show_multi_digit_number)


class Instructions26TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    def get_timeout_seconds(player):
        session = player.session
        return session.config['timeout_seconds_social_info']

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            square_in_01='cognitive_load_exp/example_blocks_type_b/square/square_in_01.png',
            square_in_09='cognitive_load_exp/example_blocks_type_b/square/square_in_09.png',
            square_out_01='cognitive_load_exp/example_blocks_type_b/square/square_out_01.png',
            square_out_09='cognitive_load_exp/example_blocks_type_b/square/square_out_09.png',
            triangle_in_01='cognitive_load_exp/example_blocks_type_b/triangle/triangle_in_01.png',
            triangle_in_09='cognitive_load_exp/example_blocks_type_b/triangle/triangle_in_09.png',
            triangle_out_01='cognitive_load_exp/example_blocks_type_b/triangle/triangle_out_01.png',
            triangle_out_09='cognitive_load_exp/example_blocks_type_b/triangle/triangle_out_09.png',
        )


class Instructions27TypeB(Page):
    form_model = 'player'
    form_fields = ['urn_choice']
    """Only show page to Type B"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            image_path='cognitive_load_exp/example_blocks_type_b/urn_choice.png',
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        participant.urn_choice = player.field_maybe_none('urn_choice')
        player.urn_choice = None


class Instructions28TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    timeout_seconds = 4

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    @staticmethod
    def vars_for_template(player: Player):
        return dict(
            image_path='cognitive_load_exp/example_blocks_type_b/left_urn_chosen_soc.png',
            image_path2='cognitive_load_exp/example_blocks_type_b/right_urn_chosen_soc.png',
        )


class Instructions29TypeB(Page):
    form_model = 'player'
    form_fields = ['insert_memorize_number']
    """Only show page to Subjects with treatment cognitive load = yes:"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    """Register whether a given player inserted the correct number they had to memorize and store in boolean variable
    'memorized_correctly' defined in models.py under class Player:"""

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        """Register whether a player was able to correctly remember the multi-digit number in player variable
        'memorized_correctly' (Correct=1, Wrong=0):"""
        participant = player.participant
        participant.memorize_number = player.field_maybe_none('memorize_number')
        participant.insert_memorize_number = player.field_maybe_none('insert_memorize_number')
        player.recalled_number = player.field_maybe_none('insert_memorize_number')
        participant.recalled_number = player.field_maybe_none('recalled_number')
        print('memorize_number is:', player.field_maybe_none('memorize_number'))
        print('insert_memorize_number is:', player.field_maybe_none('insert_memorize_number'))
        print('recalled_number is:', player.field_maybe_none('recalled_number'))
        if player.field_maybe_none('memorize_number') == player.field_maybe_none('insert_memorize_number'):
            player.memorized_correctly = 1
            print('the focal player memorized the number correctly')
        else:
            player.memorized_correctly = 0
            print('the focal player did not memorize the number correctly')
        """Before next page make the player variable 'insert_memorize_number' blank again to avoid that a default number
        is already inserted when a player has to insert a new number on one of the following pages again:"""
        player.insert_memorize_number = ""


class Instructions30TypeB(Page):
    form_model = 'player'
    """Only show page to Type B"""

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1


class QuizQuestion1(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['question1']


class QuizQuestion2(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['question2']


class QuizQuestion3(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['question3']


class QuizQuestion4TypeA(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['question4_type_a']


class QuizQuestion4TypeB(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['question4_type_b']


class QuizQuestion5TypeA(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['question5_type_a']


class QuizQuestion5TypeB(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['question5_type_b']


class QuizQuestion6TypeA(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['question6_type_a']


class QuizQuestion6TypeB(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['question6_type_b']


class QuizQuestion7TypeB(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and player.round_number == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['question7_type_b']


class InstructionsFinal(WaitPage):
    form_model = 'player'

    wait_for_all_groups = True

    """Specify to only show page in the first round:"""
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1


############################################################################################################
#                                              EXPERIMENT                                                  #
############################################################################################################


class Task0(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        """Remove data that was registered during instructions because instructions was round 1 and we continue with round 1 in actual 
        experiment where we want to have a clean sheet of choice data and only keep treatment assignments, etc.:"""
        player.urn_choice = None
        participant.urn_choice = player.field_maybe_none('urn_choice')
        player.memorized_correctly = None
        participant.memorized_correctly = player.field_maybe_none('memorized_correctly')
        player.memorize_number = None


class TaskTypeA1(Page):
    form_model = 'player'
    form_fields = ['urn_choice']
    """Only show page to Type A"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number >= 1

    @staticmethod
    def vars_for_template(player: Player):
        block_number = round((player.round_number / 4)+0.26)
        round_in_block = player.round_number-((block_number-1)*player.subsession.session.config['num_rounds_per_block'])
        return dict(
            image_path='cognitive_load_exp/example_block_type_a/urn_choice.png',
            round_in_block=round_in_block,
            block_number=block_number,
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        participant.urn_choice = player.urn_choice


class DetermineColorDrawnmarbleTypeA(WaitPage):
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number >= 1

    form_model = 'player'
    title_text = "Please wait"
    body_text = (
        "We will soon show you the color of the drawn marble but we first wait for every type A participant "
        "to choose an urn."
    )

    after_all_players_arrive = 'color_and_payoff_type_a'

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        participant.optimal_urn = player.optimal_urn


class TaskTypeA2(Page):
    form_model = 'player'
    """Only show page to Type A"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and player.round_number >= 1

    @staticmethod
    def vars_for_template(player: Player):
        block_number = round((player.round_number / 4)+0.26)
        round_in_block = player.round_number-((block_number-1)*player.subsession.session.config['num_rounds_per_block'])
        return dict(
            round_in_block=round_in_block,
            block_number=block_number,
            wc_red_left_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_left_red_100.png',
            wc_red_left_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_left_blue_0.png',
            wc_red_right_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_right_red_100.png',
            wc_red_right_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_red/urn_choice_right_blue_0.png',
            wc_blue_left_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_left_red_0.png',
            wc_blue_left_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_left_blue_100.png',
            wc_blue_right_urn_red_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_right_red_0.png',
            wc_blue_right_urn_blue_marble='cognitive_load_exp/example_block_type_a/wc_blue/urn_choice_right_blue_100.png',
        )


class WaitForTypeA(WaitPage):
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B

    wait_for_all_groups = True

    form_model = 'player'

    title_text = "Please wait"
    body_text = (
        "Please wait, until all type A participants have chosen between the two urns for 4 rounds."
    )
    after_all_players_arrive = 'choice_dist_and_treat_assign'

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        participant.similar_ingroup = player.similar_ingroup
        participant.observing_ingroup = player.observing_ingroup


class TaskTypeBReady(Page):
    form_model = 'player'

    timeout_seconds = 2

    """Type B participants only  choose after Type A participants have chosen for 4 rounds. Thus, Type B will only see page every fourth 
    round and I use the modulo operator % to do this:"""
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and (player.round_number % 4 == 0)


class TaskTypeBCogLoad(Page):
    form_model = 'player'

    def get_timeout_seconds(player):
        session = player.session
        return session.config['timeout_seconds_multi_digit']

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and (player.round_number % 4 == 0)

    @staticmethod
    def vars_for_template(player: Player):
        import random
        participant = player.participant
        subsession = player.subsession
        block_number = round(subsession.round_number / 4)
        length_multi_digit = player.session.length_multi_digit
        print('number of digits for this participant to memorize is:', length_multi_digit)
        multi_digit_number = [None] * length_multi_digit
        for i in range(0, length_multi_digit):
            multi_digit_number[i] = random.randrange(1, 9, 1)
        """First define a variable that shows the four list elements as separate characters/string-elements/letters 
        with spaces between the separate letters:"""
        show_multi_digit_number = ' '.join([str(elem) for elem in multi_digit_number])
        """Then do same but without spaces so that we can then compare this number to the number the subject will insert
        on template of the page 'Instructions32TypeB':"""
        player.memorize_number = ''.join([str(elem) for elem in multi_digit_number])
        participant.memorize_number = player.memorize_number
        print('player.memorize_number', player.memorize_number)
        print('participant.memorize_number', participant.memorize_number)
        """Take variable above with spaces (show_multi_digit_number) and return in dictionary:"""
        return dict(
            show_multi_digit_number=show_multi_digit_number,
            block_number=block_number,
        )


class TaskTypeB1(Page):
    form_model = 'player'

    def get_timeout_seconds(player):
        session = player.session
        return session.config['timeout_seconds_social_info']

    """Type B participants only  choose after Type A participants have chosen for 4 rounds. Thus, Type B will only see page every fourth 
    round and I use the modulo operator % to do this:"""
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and (player.round_number % 4 == 0)

    @staticmethod
    def vars_for_template(player: Player):
        subsession = player.subsession
        number_left_urn_triangle = subsession.number_left_urn_triangle
        number_left_urn_square = subsession.number_left_urn_square
        block_number = round(subsession.round_number / 4)
        return dict(
            number_left_urn_triangle=number_left_urn_triangle,
            number_left_urn_square=number_left_urn_square,
            block_number=block_number,
            triangle_left1_right4='cognitive_load_exp/social_info/triangle/triangle_left1_right4.png',
            triangle_left2_right3='cognitive_load_exp/social_info/triangle/triangle_left2_right3.png',
            triangle_left3_right2='cognitive_load_exp/social_info/triangle/triangle_left3_right2.png',
            triangle_left4_right1='cognitive_load_exp/social_info/triangle/triangle_left4_right1.png',
            triangle_left5='cognitive_load_exp/social_info/triangle/triangle_left5.png',
            triangle_right5='cognitive_load_exp/social_info/triangle/triangle_right5.png',
            square_left1_right4='cognitive_load_exp/social_info/square/square_left1_right4.png',
            square_left2_right3='cognitive_load_exp/social_info/square/square_left2_right3.png',
            square_left3_right2='cognitive_load_exp/social_info/square/square_left3_right2.png',
            square_left4_right1='cognitive_load_exp/social_info/square/square_left4_right1.png',
            square_left5='cognitive_load_exp/social_info/square/square_left5.png',
            square_right5='cognitive_load_exp/social_info/square/square_right5.png',
            square_01='cognitive_load_exp/similarity/square_01.png',
            square_09='cognitive_load_exp/similarity/square_09.png',
            triangle_01='cognitive_load_exp/similarity/triangle_01.png',
            triangle_09='cognitive_load_exp/similarity/triangle_09.png',
        )


class TaskTypeB2(Page):
    form_model = 'player'
    form_fields = ['urn_choice']
    """Only show the page to Type A"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and (player.round_number % 4 == 0)

    @staticmethod
    def vars_for_template(player: Player):
        subsession = player.subsession
        block_number = round(subsession.round_number / 4)
        return dict(
            image_path='cognitive_load_exp/example_block_type_a/urn_choice.png',
            block_number=block_number,
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        participant.urn_choice = player.urn_choice


class TaskTypeB2Second(Page):
    form_model = 'player'
    """Only show page to Type A"""

    timeout_seconds = 4

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and (player.round_number % 4 == 0)

    @staticmethod
    def vars_for_template(player: Player):
        subsession = player.subsession
        block_number = round(subsession.round_number / 4)
        return dict(
            block_number=block_number,
            left_urn_chosen_soc='cognitive_load_exp/example_blocks_type_b/left_urn_chosen_soc.png',
            right_urn_chosen_soc='cognitive_load_exp/example_blocks_type_b/right_urn_chosen_soc.png',
        )


class TaskTypeB3(Page):
    form_model = 'player'
    form_fields = ['insert_memorize_number']
    """Only show page to Subjects with treatment cognitive load = yes:"""

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and (player.round_number % 4 == 0)

    """Register whether a given player inserted the correct number they had to memorize and store in boolean variable
    'memorized_correctly' defined in models.py under class Player:"""

    @staticmethod
    def vars_for_template(player: Player):
        subsession = player.subsession
        block_number = round(subsession.round_number / 4)
        return dict(
            block_number=block_number,
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        """Register whether a player was able to correctly remember the multi-digit number in player variable
        'memorized_correctly' (Correct=1, Wrong=0):"""
        participant = player.participant
        participant.memorize_number = player.field_maybe_none('memorize_number')
        participant.insert_memorize_number = player.field_maybe_none('insert_memorize_number')
        player.recalled_number = player.field_maybe_none('insert_memorize_number')
        participant.recalled_number = player.field_maybe_none('recalled_number')
        print('memorize_number is:', player.memorize_number)
        print('insert_memorize_number is:', player.insert_memorize_number)
        print('recalled_number is:', participant.recalled_number)
        if player.field_maybe_none('memorize_number') == player.field_maybe_none('insert_memorize_number'):
            player.memorized_correctly = 1
            print('the focal player memorized the number correctly')
        else:
            player.memorized_correctly = 0
            print('the focal player did not memorize the number correctly')
        participant.memorized_correctly = player.memorized_correctly
        """Before next page make the player variable 'insert_memorize_number' blank again to avoid that a default number
        is already inserted when a player has to insert a new number on one of the following pages again:"""
        player.insert_memorize_number = ""


class TypeBWaitForTypeB(WaitPage):
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_B and (player.round_number % 4 == 0)

    wait_for_all_groups = True

    form_model = 'player'
    title_text = "Please wait"
    body_text = (
        "Please wait, until all type B participants have chosen between the two urns."
    )

    after_all_players_arrive = 'color_and_payoff_type_b'


class WaitForTypeB(WaitPage):
    """Type A participants wait with proceeding to 5th period until all type B participants have chosen:"""
    @staticmethod
    def is_displayed(player: Player):
        return player.participant.type_assignment == C.TYPE_A and (player.round_number % 4 == 0)

    wait_for_all_groups = True

    form_model = 'player'
    title_text = "Please wait"
    body_text = (
        "Please wait, until all type B participants have finished their decision tasks."
    )


class Questionnaire1Test(Page):
    @staticmethod
    def is_displayed(player: Player):
        participant = player.participant
        return player.participant.type_assignment == C.TYPE_B

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['questionnaire1_test']

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        """Register answers to questionnaire:"""
        participant = player.participant
        participant.q1 = player.field_maybe_none('q1')


class Questionnaire1(Page):
    @staticmethod
    def is_displayed(player: Player):
        participant = player.participant
        return player.round_number == player.subsession.session.config['num_rounds_total'] and player.participant.type_assignment == C.TYPE_B

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['q1']

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        """Register answers to questionnaire:"""
        participant = player.participant
        participant.q1 = player.field_maybe_none('q1')


class Questionnaire2(Page):
    @staticmethod
    def is_displayed(player: Player):
        participant = player.participant
        return player.round_number == player.subsession.session.config['num_rounds_total'] and player.participant.type_assignment == C.TYPE_B

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['q2']

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        """Register answers to questionnaire:"""
        participant = player.participant
        participant.q2 = player.field_maybe_none('q2')


class Questionnaire3(Page):
    @staticmethod
    def is_displayed(player: Player):
        participant = player.participant
        return player.round_number == player.subsession.session.config['num_rounds_total'] and player.participant.type_assignment == C.TYPE_B

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['q3']

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        """Register answers to questionnaire:"""
        participant = player.participant
        participant.q3 = player.field_maybe_none('q3')


class Questionnaire4NOCL(Page):
    @staticmethod
    def is_displayed(player: Player):
        participant = player.participant
        return player.round_number == player.subsession.session.config['num_rounds_total'] and player.participant.type_assignment == C.TYPE_B and player.participant.cognitive_load == 0

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['q4nocl']

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        """Register answers to questionnaire:"""
        participant = player.participant
        participant.q4nocl = player.field_maybe_none('q4nocl')


class Questionnaire4(Page):
    @staticmethod
    def is_displayed(player: Player):
        participant = player.participant
        return player.round_number == player.subsession.session.config['num_rounds_total'] and player.participant.type_assignment == C.TYPE_B and player.participant.cognitive_load == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['q4']

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        """Register answers to questionnaire:"""
        participant = player.participant
        participant.q4 = player.field_maybe_none('q4')


class Questionnaire5NOCL(Page):
    @staticmethod
    def is_displayed(player: Player):
        participant = player.participant
        return player.round_number == player.subsession.session.config['num_rounds_total'] and player.participant.type_assignment == C.TYPE_B and player.participant.cognitive_load == 0

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['q5nocl']

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        """Register answers to questionnaire:"""
        participant = player.participant
        participant.q5nocl = player.field_maybe_none('q5nocl')


class Questionnaire5(Page):
    @staticmethod
    def is_displayed(player: Player):
        participant = player.participant
        return player.round_number == player.subsession.session.config['num_rounds_total'] and player.participant.type_assignment == C.TYPE_B and player.participant.cognitive_load == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['q5']

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        """Register answers to questionnaire:"""
        participant = player.participant
        participant.q5 = player.field_maybe_none('q5')


class Questionnaire6(Page):
    @staticmethod
    def is_displayed(player: Player):
        participant = player.participant
        return player.round_number == player.subsession.session.config['num_rounds_total'] and player.participant.type_assignment == C.TYPE_B and player.participant.cognitive_load == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['q6']

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        """Register answers to questionnaire:"""
        participant = player.participant
        participant.q6 = player.field_maybe_none('q6')


class Questionnaire7(Page):
    @staticmethod
    def is_displayed(player: Player):
        participant = player.participant
        return player.round_number == player.subsession.session.config['num_rounds_total'] and player.participant.type_assignment == C.TYPE_B and player.participant.cognitive_load == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['q7']

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        """Register answers to questionnaire:"""
        participant = player.participant
        participant.q7 = player.field_maybe_none('q7')


class Questionnaire8(Page):
    @staticmethod
    def is_displayed(player: Player):
        participant = player.participant
        return player.round_number == player.subsession.session.config['num_rounds_total'] and player.participant.type_assignment == C.TYPE_B and player.participant.cognitive_load == 1

    # This block determines which comp check questions to ask the user.
    form_model = 'player'
    form_fields = ['q8']

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        """Register answers to questionnaire:"""
        participant = player.participant
        participant.q8 = player.field_maybe_none('q8')
        player.monetary_earnings = player.participant.payoff.to_real_world_currency(player.participant.session) + player.session.participation_fee
        participant.monetary_earnings = player.monetary_earnings


class Demographics(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == player.subsession.session.config['num_rounds_total']

    form_model = 'player'
    form_fields = ['age', 'gender', 'study_subject']


class Thank(Page):
    form_model = 'player'

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == player.subsession.session.config['num_rounds_total']

    @staticmethod
    def vars_for_template(player):
        money = player.participant.payoff.to_real_world_currency(player.participant.session)
        final_earnings = player.session.participation_fee + money
        return dict(
            money=money,
            final_earnings=final_earnings,
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        """Register bank account in participant variable:"""
        participant = player.participant
        player.monetary_earnings = player.participant.payoff.to_real_world_currency(player.participant.session) + player.session.participation_fee
        participant.monetary_earnings = player.monetary_earnings


class PaymentInfo(Page):
    form_model = 'player'

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == player.subsession.session.config['num_rounds_total']

    @staticmethod
    def vars_for_template(player):
        participant = player.participant
        final_earnings = round(participant.monetary_earnings)
        money = round(participant.monetary_earnings - player.session.participation_fee)
        return dict(
            money=money,
            final_earnings=final_earnings,
        )


"""
page_sequence = [InstructionsFinal,
                 Task0,
                 TaskTypeA1,
                 DetermineColorDrawnmarbleTypeA,
                 TaskTypeA2,
                 WaitForTypeA,
                 TaskTypeBReady,
                 TaskTypeBCogLoad,
                 TaskTypeB1,
                 TaskTypeB2,
                 TaskTypeB2Second,
                 TaskTypeB3,
                 WaitForTypeB,
                 TypeBWaitForTypeB,
                 Questionnaire1,
                 Questionnaire2,
                 Questionnaire3,
                 Questionnaire4,
                 Questionnaire4NOCL,
                 Questionnaire5,
                 Questionnaire5NOCL,
                 Questionnaire6,
                 Questionnaire7,
                 Questionnaire8,
                 Demographics,
                 Thank,
                 PaymentInfo
                 ]
"""


page_sequence = [
                 Consent,
                 ConsentAgree,
                 Instructions1,
                 Instructions2,
                 Instructions3,
                 Instructions4,
                 # The following page is no longer needed: Instructions5,
                 Instructions6,
                 Instructions7,
                 Instructions8,
                 Instructions9TypeA,
                 Instructions9TypeB,
                 Instructions10TypeA,
                 Instructions10TypeBFirst,
                 Instructions10TypeBSecond,
                 Instructions11TypeA,
                 Instructions11TypeB,
                 Instructions12TypeA,
                 Instructions12TypeB,
                 Instructions13,
                 # This is redundant after seeing the figure showing the sequence: Instructions14,
                 # This is redundant after seeing the figure showing the sequence: Instructions15,
                 Instructions16TypeA,
                 Instructions16TypeB,
                 Instructions17TypeA,
                 Instructions17TypeB,
                 Instructions18TypeA,
                 Instructions18TypeB,
                 Instructions18TypeB2,
                 Instructions18TypeB3,
                 Instructions19TypeA,
                 Instructions19TypeB,
                 Instructions19TypeBSocInfo1,
                 Instructions19TypeBSocInfo2,
                 Instructions19TypeBSocInfo3,
                 Instructions19TypeBSocInfo4,
                 Instructions20TypeA,
                 DetermineColorDrawnmarbleTypeAPracticeBlock,
                 Instructions21TypeA,
                 Instructions20TypeARound2,
                 DetermineColorDrawnmarbleTypeAPracticeBlockRound2,
                 Instructions21TypeARound2,
                 Instructions20TypeARound3,
                 DetermineColorDrawnmarbleTypeAPracticeBlockRound3,
                 Instructions21TypeARound3,
                 Instructions20TypeARound4,
                 DetermineColorDrawnmarbleTypeAPracticeBlockRound4,
                 Instructions21TypeARound4,
                 Instructions20TypeB,
                 Instructions21TypeB,
                 Instructions21TypeB2,
                 Instructions22TypeB,
                 Instructions23TypeB,
                 Instructions24TypeB,
                 Instructions25TypeB,
                 Instructions26TypeB,
                 Instructions27TypeB,
                 Instructions28TypeB,
                 Instructions29TypeB,
                 Instructions30TypeB,
                 QuizQuestion1,
                 QuizQuestion2,
                 QuizQuestion3,
                 QuizQuestion4TypeA,
                 QuizQuestion4TypeB,
                 QuizQuestion5TypeA,
                 QuizQuestion5TypeB,
                 QuizQuestion6TypeA,
                 QuizQuestion6TypeB,
                 QuizQuestion7TypeB,
                 InstructionsFinal,
                 Task0,
                 TaskTypeA1,
                 DetermineColorDrawnmarbleTypeA,
                 TaskTypeA2,
                 WaitForTypeA,
                 TaskTypeBReady,
                 TaskTypeBCogLoad,
                 TaskTypeB1,
                 TaskTypeB2,
                 TaskTypeB2Second,
                 TaskTypeB3,
                 WaitForTypeB,
                 TypeBWaitForTypeB,
                 Questionnaire1,
                 Questionnaire2,
                 Questionnaire3,
                 Questionnaire4,
                 Questionnaire4NOCL,
                 Questionnaire5,
                 Questionnaire5NOCL,
                 Questionnaire6,
                 Questionnaire7,
                 Questionnaire8,
                 Demographics,
                 Thank,
                 PaymentInfo
                 ]    


