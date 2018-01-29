<apply template="base">
    <h2 class="ui header">Add Coffee</h2>
    <br>

    <dfForm action="/new_bean" class="fluid ui form">
        <dfChildErrorList />

        <div class="ui grid">
            <div class="sixteen wide field">
                <dfLabel ref="name">Coffee</dfLabel>
                <dfInputText ref="name" class="ui fluid" />
            </div>
        </div>

        <div class="ui grid">
            <div class="sixteen wide column">
                <div class="sixteen wide field">
                    <dfLabel ref="roaster">Roaster</dfLabel>
                    <dfInputText ref="roaster" class="ui fluid" />
                </div>
            </div>
        </div>

        <div class="ui grid">
            <dfInputSubmit value="Save" class="fluid ui button" />
        </div>
    </dfForm>
</apply>
