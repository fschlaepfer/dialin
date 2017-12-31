<apply template="base">
    <h2 class="ui header">Pull Shot</h2>
    <br>

    <dfForm action="/new" class="fluid ui form">
        <dfChildErrorList />

        <div class="ui grid">
            <div class="ten wide field">
                <dfLabel ref="beanid">Bean</dfLabel>
                <dfInputSelect class="ui fluid dropdown" ref="beanid" />
            </div>

            <div class="six wide field">
                <dfLabel ref="grind">Grind</dfLabel>
                <dfInputSelect class="ui fluid dropdown" ref="grind" />
            </div>
        </div>

        <div class="ui grid">
            <div class="four wide field">
                <dfLabel ref="dose">Dose [g]</dfLabel>
                <dfInput ref="dose" type="number" min="0" step="1" pattern="\d+" value="18" />
            </div>

            <div class="four wide field">
                <dfLabel ref="time">Time [s]</dfLabel>
                <dfInput ref="time" type="number" min="0" step="1" pattern="\d+" value="28" />
            </div>

            <div class="four wide field">
                <dfLabel ref="yield">Yield [g]</dfLabel>
                <dfInput ref="yield" type="number" min="0" step="1" pattern="\d+" value="36" />
            </div>

            <div class="four wide field">
                <dfLabel ref="temperature">Temp [°C]</dfLabel>
                <dfInput ref="temperature" type="number" min="0" step="1" pattern="\d+" value="92" />
            </div>
        </div>
        <br/>

        <div class="ui grid">
            <div class="six wide field">
                <dfLabel ref="rating">Rating</dfLabel>
                <dfInputSelect class="ui fluid dropdown" ref="rating" />
            </div>

            <div class="ten wide field">
                <dfLabel ref="notes">Notes</dfLabel>
                <dfInputText ref="notes" />
            </div>
        </div>

        <div class="ui grid">
            <dfInputSubmit class="fluid ui button" value="Save" />
        </div>
    </dfForm>
</apply>
