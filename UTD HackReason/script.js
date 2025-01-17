// Fetch appliance states from the server and populate the table
function fetchApplianceStates() {
    fetch('/appliance_states')
      .then(response => response.json())
      .then(data => {
        const tableBody = document.getElementById('appliance-table');
        tableBody.innerHTML = ''; // Clear existing rows
        data.forEach(appliance => {
          const row = document.createElement('tr');
          row.innerHTML = `
            <td>${appliance.name}</td>
            <td>${appliance.state}</td>
            <td>${appliance.room}</td>
            <td><button class="button" onclick="toggleAppliance('${appliance.name}', '${appliance.state}')">Toggle</button></td>
          `;
          tableBody.appendChild(row);
        });
      })
      .catch(error => console.error('Error fetching appliance states:', error));
  }
  
  // Toggle an appliance's state
  function toggleAppliance(name, currentState) {
    const newState = currentState === "on" ? "off" : "on"; // Calculate new state
    fetch('/toggle_appliance', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ name, state: newState }) // Send new state to the server
    })
      .then(response => response.json())
      .then(data => {
        if (data.status === "success") {
          alert(`Appliance ${name} is now ${data.new_state}`);
          fetchApplianceStates(); // Refresh appliance list
        } else {
          alert(`Error: ${data.message}`);
        }
      })
      .catch(error => console.error('Error toggling appliance:', error));
  }
  
  // Load appliance states on page load
  window.onload = fetchApplianceStates;
  
