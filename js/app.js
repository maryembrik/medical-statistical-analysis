let currentStep = 1;

function updateProgress(value) {
    document.getElementById("progress").style.width = value + "%";
}

function updateStep(step) {
    for (let i = 1; i <= 3; i++) {
        document.getElementById("dot" + i).classList.remove("active");
    }
    for (let i = 1; i <= step; i++) {
        document.getElementById("dot" + i).classList.add("active");
    }
}

document.getElementById("step1").innerHTML = `
    <div class="question">What type of data are you analyzing?</div>
    <div class="options">
        <div class="option-card" onclick="selectData('categorical')">
            <div class="option-title">Categorical Data</div>
            <div class="option-desc">Nominal or ordinal variables</div>
        </div>
        <div class="option-card" onclick="selectData('numerical')">
            <div class="option-title">Numerical Data</div>
            <div class="option-desc">Continuous measurements</div>
        </div>
    </div>
`;

function selectData(type) {
    updateProgress(33);
    updateStep(2);
    document.getElementById("step1").style.display = "none";
    document.getElementById("step2").style.display = "block";

    if (type === "categorical") {
        showResult("Chi-Square Test");
    } else {
        document.getElementById("step2").innerHTML = `
            <div class="question">Is the data normally distributed?</div>
            <div class="options">
                <div class="option-card" onclick="selectNormality(true)">
                    <div class="option-title">Yes</div>
                    <div class="option-desc">Normal distribution</div>
                </div>
                <div class="option-card" onclick="selectNormality(false)">
                    <div class="option-title">No</div>
                    <div class="option-desc">Skewed or small sample</div>
                </div>
            </div>
        `;
    }
}

function selectNormality(isNormal) {
    updateProgress(66);
    updateStep(3);

    if (isNormal) {
        showResult("Student’s t-test / ANOVA");
    } else {
        showResult("Mann-Whitney / Kruskal-Wallis");
    }
}

function showResult(testName) {
    updateProgress(100);
    document.getElementById("step2").style.display = "none";

    const result = document.getElementById("result");
    result.innerHTML = `
        <h2>Your recommended test</h2>
        <div class="test-name">${testName}</div>
        <p>This test is appropriate based on your data type, distribution, and study design.</p>
        <button class="reset-btn" onclick="resetTool()">Start Over</button>
    `;
    result.classList.add("show");
}

function resetTool() {
    updateProgress(0);
    updateStep(1);
    document.getElementById("step1").style.display = "block";
    document.getElementById("step2").style.display = "none";
    document.getElementById("result").classList.remove("show");
}
