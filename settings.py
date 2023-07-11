from os import environ


SESSION_CONFIGS = [
    dict(
        num_rounds_total=80,
        num_rounds_per_block=4,
        name='urn_choice_experiment',
        display_name="Urn Choice Game",
        app_sequence=['urn_choice_experiment'],
        start_value_treatment_permutations=0,
        num_demo_participants=36,
        nbr_individual_learners=10,
        winning_color_triangle='blue',
        length_multi_digit=7,
        timeout_seconds_multi_digit=8,
        timeout_seconds_social_info=12,
        red_marbles_majority=[],
        permut_treat_combinations=[],
        block_number=[],
        doc="""ATTENTION: The following packages need to be installed because I import them in the apps; 'itertools', 'random', and 'numpy'.
        Further info: In case we want to counterbalance the winning color for individual learners across sessions, we can 
        change the parameter 'winning_color_triangle' above. Similarly, we can change the length of the multi-digit number for the 
        cognitive load task, which we call 'length_multi_digit' above. We can also manipulate here the number of seconds that the
        participants in the cognitive load treatment will be shown the multi-digit number to memorize it, called 
        'timeout_seconds_multi_digit' above. We can also manipulate the number of seconds that the participants will be able to see the
        social information (choice distribution of Type A participants).""",
    ),
]

# if you set a property in SESSION_CONFIG_DEFAULTS, it will be inherited by all configs
# in SESSION_CONFIGS, except those that explicitly override it.
# the session config can be accessed from methods in your apps as self.session.config,
# e.g. self.session.config['participation_fee']

SESSION_CONFIG_DEFAULTS = dict(
    real_world_currency_per_point=0.002, participation_fee=4.5, doc=""
)

PARTICIPANT_FIELDS = ['wrong_answer_1', 'wrong_answer_2', 'wrong_answer_3', 'wrong_answer_4', 'wrong_answer_5', 'wrong_answer_6',
                      'wrong_answer_7',
                      'monetary_earnings', 'consent', 'treatment_assignment', 'cognitive_load',
                      'similar_ingroup', 'observing_ingroup', 'type_assignment', 'pay_task',
                      'group_assignment', 'urn_choice', 'optimal_urn', 'share_winning_color', 'color_drawn_marble', 'memorized_correctly',
                      'memorize_number', 'insert_memorize_number', 'recalled_number', 'color_drawn_marble_left', 'color_drawn_marble_right',
                      'q1', 'q2', 'q3', 'q4nocl', 'q4', 'q5nocl', 'q5', 'q6', 'q7', 'q8', 'age', 'gender', 'study_subject']
SESSION_FIELDS = ['length_multi_digit', 'timeout_seconds_multi_digit', 'num_rounds_total', 'num_rounds_per_block',
                  'red_majority', 'red_marbles_majority', 'number_left_urn_triangle',
                  'number_left_urn_square', 'winning_color_triangle', 'winning_color_square', 'nbr_individual_learners',
                  'my_page_timeout_seconds', 'permut_treat_combinations']

# ISO-639 code
# for example: de, fr, ja, ko, zh-hans
LANGUAGE_CODE = 'en'

# e.g. EUR, GBP, CNY, JPY
REAL_WORLD_CURRENCY_CODE = 'USD'
USE_POINTS = True

ROOMS = [
    dict(
        name='Busara',
        display_name='Busara',
    ),
    dict(name='live_demo', display_name='Room for live demo (no participant labels)'),
]

ADMIN_USERNAME = 'admin'
# for security, best to set admin password in an environment variable
ADMIN_PASSWORD = environ.get('OTREE_ADMIN_PASSWORD')

DEMO_PAGE_INTRO_HTML = """
Here are some oTree games.
"""


SECRET_KEY = '4753844873839'

INSTALLED_APPS = ['otree']
