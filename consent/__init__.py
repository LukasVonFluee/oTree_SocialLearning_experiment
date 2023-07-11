from otree.api import *


doc = """
Your app description
"""


class C(BaseConstants):
    NAME_IN_URL = 'consent'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 1


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    consent = models.BooleanField()



# PAGES
class MyPage(Page):
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


class ResultsWaitPage(WaitPage):
    pass


class Results(Page):
    pass


page_sequence = [MyPage,]
