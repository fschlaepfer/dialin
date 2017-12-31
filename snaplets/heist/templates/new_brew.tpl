<apply template="base">
    <h2>Log Shot</h2>

    <p><a href="/new">Click to view recent shots</a></p>

    <dfForm action="/new">
        <dfChildErrorList />

        <dfLabel ref="beanid">Bean: </dfLabel>
        <dfInputSelect ref="beanid" />
        <br>

        <dfLabel ref="grind">Grind: </dfLabel>
        <dfInputSelect ref="grind" />
        <br>

        <dfLabel ref="dose">Dose: </dfLabel>
        <dfInput ref="dose" type="number" min="0" step="1" pattern="\d+" value="18" />
        <br>

        <dfLabel ref="time">Time: </dfLabel>
        <dfInput ref="time" type="number" min="0" step="1" pattern="\d+" value="28" />
        <br>

        <dfLabel ref="yield">Yield: </dfLabel>
        <dfInput ref="yield" type="number" min="0" step="1" pattern="\d+" value="36" />
        <br>

        <dfLabel ref="temperature">Temperature: </dfLabel>
        <dfInput ref="temperature" type="number" min="0" step="1" pattern="\d+" value="92" />
        <br>

        <dfLabel ref="rating">Rating: </dfLabel>
        <dfInputSelect ref="rating" />
        <br>

        <dfLabel ref="notes">Notes: </dfLabel>
        <dfInputText ref="notes" />
        <br>

        <dfInputSubmit value="Save" />
    </dfForm>
</apply>
