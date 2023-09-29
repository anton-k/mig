-- | random content for blog posts
module Content (
  poems,
  quotes,
) where

import Data.Text (Text)
import Data.Text qualified as Text

poems :: [Text]
poems = [lukomorye, tweedle, dream, pie, toSeeAWorld]

lukomorye :: Text
lukomorye =
  Text.unlines
    [ "У лукоморья дуб зелёный"
    , "Златая цепь на дубе том"
    , "И днём и ночью кот учёный"
    , "Всё ходит по цепи кругом;"
    , "Идёт направо — песнь заводит,"
    , "Налево — сказку говорит."
    , "Там чудеса: там леший бродит,"
    , "Русалка на ветвях сидит;"
    , "Там на неведомых дорожках"
    , "Следы невиданных зверей;"
    , "Избушка там на курьих ножках"
    , "Стоит без окон, без дверей;"
    , "Там лес и дол видений полны;"
    , "Там о заре прихлынут волны"
    , "На брег песчаный и пустой,"
    , "И тридцать витязей прекрасных"
    , "Чредой из вод выходят ясных,"
    , "И с ними дядька их морской;"
    , "Там королевич мимоходом"
    , "Пленяет грозного царя;"
    , "Там в облаках перед народом"
    , "Через леса, через моря"
    , "Колдун несёт богатыря;"
    , "В темнице там царевна тужит,"
    , "А бурый волк ей верно служит;"
    , "Там ступа с Бабою Ягой"
    , "Идёт, бредёт сама собой,"
    , "Там царь Кащей над златом чахнет;"
    , "Там русский дух… там Русью пахнет!"
    , "И там я был, и мёд я пил;"
    , "У моря видел дуб зелёный;"
    , "Под ним сидел, и кот учёный"
    , "Свои мне сказки говорил."
    , ""
    , "Александр Пушкин"
    ]

dream :: Text
dream =
  Text.unlines
    [ "И это снилось мне, и это снится мне,"
    , "И это мне ещё когда-нибудь приснится,"
    , "И повторится всё, и всё довоплотится,"
    , "И вам приснится всё, что видел я во сне."
    , ""
    , "Там, в стороне от нас, от мира в стороне"
    , "Волна идёт вослед волне о берег биться,"
    , "А на волне звезда, и человек, и птица,"
    , "И явь, и сны, и смерть — волна вослед волне."
    , ""
    , "Не надо мне числа: я был, и есмь, и буду,"
    , "Жизнь — чудо из чудес, и на колени чуду"
    , "Один, как сирота, я сам себя кладу,"
    , "Один, среди зеркал — в ограде отражений"
    , "Морей и городов, лучащихся в чаду."
    , "И мать в слезах берёт ребёнка на колени."
    , ""
    , "Арсений Тарковский"
    ]

pie :: Text
pie =
  Text.unlines
    [ "Cottleston Cottleston Cottleston Pie"
    , "A fly can't bird, but a bird can fly."
    , "Ask me a riddle and I reply"
    , "Cottleston Cottleston Cottleston Pie."
    , ""
    , "Cottleston Cottleston Cottleston Pie,"
    , "Why does a chicken? I don't know why."
    , "Ask me a riddle and I reply"
    , "Cottleston Cottleston Cottleston Pie."
    , ""
    , "Alan Milne"
    ]

toSeeAWorld :: Text
toSeeAWorld =
  Text.unlines
    [ "To see a World in a Grain of Sand"
    , "And a Heaven in a Wild Flower,"
    , "Hold Infinity in the palm of your hand"
    , "And Eternity in an hour"
    , ""
    , "William Blake"
    ]

tweedle :: Text
tweedle =
  Text.unlines
    [ "Tweedledum and Tweedledee"
    , "Agreed to have a battle;"
    , "For Tweedledum said Tweedledee"
    , "Had spoiled his nice new rattle."
    , "Just then flew down a monstrous crow,"
    , "As black as a tar-barrel;"
    , "Which frightened both the heroes so,"
    , "They quite forgot their quarrel."
    , ""
    , "Lewis Carroll"
    ]

-------------------------------------------------------------------------------------
-- random content for quotes

quotes :: [Text]
quotes =
  [ "“We ascribe beauty to that which is simple; which has no superfluous parts; which exactly answers its end; which stands related to all things; which is the mean of many extremes.” - Ralph Waldo Emerson"
  , "“Each day a few more lies eat into the seed with which we are born, little institutional lies from the print of newspapers, the shock waves of television, and the sentimental cheats of the movie screen.” - Norman Mailer"
  , "“Let us so live that when we come to die even the undertaker will be sorry.” - Mark Twain"
  , "“When we seek to discover the best in others, we somehow bring out the best in ourselves.” - William Arthur Ward"
  , "“Make things as simple as possible, but not simpler.” - Albert Einstein"
  , "“Go often to the house of thy friend, for weeds choke the unused path.” - Ralph Waldo Emerson"
  , "“Hating people is like burning down your own house to get rid of a rat.” - Henry Emerson Fosdick"
  , "“The most pitiful among men is he who turns his dreams into silver and gold.” - Kahlil Gibran"
  , "“Persistent people begin their success where others end in failures.” - Edward Eggleston"
  , "“Pick battles big enough to matter, small enough to win.” - Jonathan Kozol"
  , "“Show me a man with both feet on the ground, and I’ll show you a man who can’t put his pants on.” - Arthur K. Watson"
  , "“The more original a discovery, the more obvious it seems afterward.” - Arthur Koestler"
  , "“Loyalty to a petrified opinion never yet broke a chain or freed a human soul.” - Mark Twain"
  , "“O senseless man, who cannot possibly make a worm and yet will make Gods by the dozen!” - Michel de Montaigne"
  , "“Talent develops in tranquility, character in the full current of human life.” - Johann Wolfgang von Goethe"
  , "“The happiness of your life depends upon the quality of your thoughts: therefore, guard accordingly, and take care that you entertain no notions unsuitable to virtue and reasonable nature.” - Marcus Aurelius"
  , "“Character cannot be developed in ease and quiet. Only through experience of trial and suffering can the soul be strengthened, ambition inspired, and success achieved.” - Helen Adams Keller"
  , "“There is nothing with which every man is so afraid as getting to know how enormously much he is capable of doing and becoming.” - Soren Kierkegaard"
  , "“If you want to build a ship, don’t drum up people together to collect wood and don’t assign them tasks and work, but rather teach them to long for the endless immensity of the sea.” - Antoine de Saint-Exupéry"
  , "“Inside every large program is a small program struggling to get out.” - Tony Hoare"
  , "“Man is most nearly himself when he achieves the seriousness of a child at play.” - Heraclitus"
  , "“Most of us are just about as happy as we make up our minds to be.” - Abraham Lincoln"
  ]
